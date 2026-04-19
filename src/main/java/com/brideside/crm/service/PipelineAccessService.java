package com.brideside.crm.service;

import com.brideside.crm.entity.Category;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Pipeline;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.User;
import com.brideside.crm.repository.DealRepository;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.repository.PipelineRepository;
import com.brideside.crm.repository.TeamRepository;
import com.brideside.crm.repository.UserRepository;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Resolves which pipelines (and related deal rows) the current user may access.
 * <ul>
 *   <li>Admin: all pipelines / no deal filter</li>
 *   <li>Category manager: if {@code users.user_managed_category_id} is set, only that vertical
 *       ({@code pipelines.category}, deal {@code category_id}, org business unit category}) — not other
 *       categories from owned orgs. If unset: organizations owned by themselves and their reports
 *       (same org tree as organization list), including deals in those orgs.</li>
 *   <li>Sales / Pre-sales: pipelines for teams where the user is a team manager and/or team member</li>
 * </ul>
 */
@Service
public class PipelineAccessService {

    private final UserRepository userRepository;
    private final OrganizationRepository organizationRepository;
    private final TeamRepository teamRepository;
    private final PipelineRepository pipelineRepository;
    private final DealRepository dealRepository;

    public PipelineAccessService(UserRepository userRepository,
                                 OrganizationRepository organizationRepository,
                                 TeamRepository teamRepository,
                                 PipelineRepository pipelineRepository,
                                 DealRepository dealRepository) {
        this.userRepository = userRepository;
        this.organizationRepository = organizationRepository;
        this.teamRepository = teamRepository;
        this.pipelineRepository = pipelineRepository;
        this.dealRepository = dealRepository;
    }

    public Optional<User> currentUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !(authentication.getPrincipal() instanceof UserDetails)) {
            return Optional.empty();
        }
        String email = ((UserDetails) authentication.getPrincipal()).getUsername();
        return userRepository.findByEmail(email);
    }

    /**
     * Pipelines the current user may see (e.g. pipeline picker, board). Empty when none.
     * Unrestricted callers should use {@link PipelineRepository#findByDeletedFalseOrderByNameAsc()} directly
     * or check {@link DealAccessScope#unrestricted()}.
     */
    public List<Pipeline> listAccessiblePipelinesOrdered() {
        DealAccessScope scope = resolveDealAccessScope();
        if (scope.fullAccess()) {
            return pipelineRepository.findByDeletedFalseOrderByNameAsc();
        }
        if (scope.allowedPipelineIds().isEmpty()) {
            return List.of();
        }
        List<Pipeline> list = pipelineRepository.findAllById(scope.allowedPipelineIds());
        list.removeIf(p -> Boolean.TRUE.equals(p.getDeleted()));
        list.sort(Comparator.comparing(Pipeline::getName, String.CASE_INSENSITIVE_ORDER));
        return list;
    }

    public DealAccessScope resolveDealAccessScope() {
        Optional<User> userOpt = currentUser();
        if (userOpt.isEmpty() || userOpt.get().getRole() == null) {
            return DealAccessScope.noAccess();
        }
        return resolveDealAccessScopeForUser(userOpt.get());
    }

    /**
     * Same visibility rules as {@link #resolveDealAccessScope()} for an explicit user
     * (e.g. category-manager dashboard without re-resolving the security principal).
     */
    public DealAccessScope resolveDealAccessScopeForUser(User user) {
        if (user == null || user.getRole() == null) {
            return DealAccessScope.noAccess();
        }
        Role.RoleName roleName = user.getRole().getName();

        if (roleName == Role.RoleName.ADMIN) {
            return DealAccessScope.unrestricted();
        }

        if (roleName == Role.RoleName.CATEGORY_MANAGER) {
            Category managed = user.getManagedCategory();
            boolean hasVertical = managed != null && managed.getName() != null && !managed.getName().isBlank();

            /*
             * Vertical is set (user_managed_category_id): scope ONLY by that product category — do not merge
             * org-owner pipelines/deals from other categories (that union caused cross-vertical leakage).
             */
            if (hasVertical) {
                String categoryName = managed.getName().trim();
                DealAccessScope.CategoryManagerCategoryScope cmScope =
                        new DealAccessScope.CategoryManagerCategoryScope(managed.getId(), categoryName);
                Set<Long> pipelineIds = new LinkedHashSet<>();
                // Only pipelines whose row matches this vertical (`pipelines.category`).
                // Do NOT union findDistinctPipelineIdsByDealCategoryId: that pulled any pipeline_id present on
                // deals tagged with this category — including Makeup/other pipelines when deals point at the wrong pipeline.
                pipelineRepository.findByDeletedFalseAndCategoryIgnoreCase(categoryName).forEach(p -> {
                    if (p.getId() != null) {
                        pipelineIds.add(p.getId());
                    }
                });
                return new DealAccessScope(false, pipelineIds, Set.of(), false, cmScope);
            }

            // No vertical: legacy category manager — organizations they own + reports (same as org list).
            List<Long> ownerIds = findAccessibleOwnerIdsForCategoryManager(user);
            Set<Long> orgIds = new LinkedHashSet<>();
            Set<Long> pipelineIds = new LinkedHashSet<>();

            if (!ownerIds.isEmpty()) {
                List<Organization> orgs = organizationRepository.findByOwner_IdIn(ownerIds);
                orgIds.addAll(orgs.stream()
                        .map(Organization::getId)
                        .filter(Objects::nonNull)
                        .collect(Collectors.toSet()));
                if (!orgIds.isEmpty()) {
                    pipelineRepository.findByDeletedFalseAndOrganization_IdIn(orgIds).forEach(p -> {
                        if (p.getId() != null) {
                            pipelineIds.add(p.getId());
                        }
                    });
                    List<Long> fromDealsOrg = dealRepository.findDistinctPipelineIdsByDealOrganizationIds(orgIds);
                    if (fromDealsOrg != null) {
                        pipelineIds.addAll(fromDealsOrg);
                    }
                }
            }

            if (orgIds.isEmpty()) {
                return DealAccessScope.noAccess();
            }
            return new DealAccessScope(false, pipelineIds, orgIds, true, null);
        }

        if (roleName == Role.RoleName.SALES || roleName == Role.RoleName.PRESALES) {
            Set<Long> teamIds = new HashSet<>();
            if (roleName == Role.RoleName.SALES) {
                teamRepository.findByManager_Id(user.getId()).forEach(t -> {
                    if (t.getId() != null) {
                        teamIds.add(t.getId());
                    }
                });
                teamRepository.findByMembers_Id(user.getId()).forEach(t -> {
                    if (t.getId() != null) {
                        teamIds.add(t.getId());
                    }
                });
            } else {
                teamRepository.findByMembers_Id(user.getId()).forEach(t -> {
                    if (t.getId() != null) {
                        teamIds.add(t.getId());
                    }
                });
            }
            if (teamIds.isEmpty()) {
                return DealAccessScope.noAccess();
            }
            List<Pipeline> pipelines = pipelineRepository.findByDeletedFalseAndTeam_IdInOrderByNameAsc(new ArrayList<>(teamIds));
            Set<Long> pipelineIds = pipelines.stream()
                    .map(Pipeline::getId)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toSet());
            // Deals often have organization_id set but pipeline_id left null (legacy / partial saves).
            // Allow those when the org matches any pipeline the user can reach via their team(s).
            Set<Long> orgIdsFromThosePipelines = pipelines.stream()
                    .map(Pipeline::getOrganization)
                    .filter(Objects::nonNull)
                    .map(Organization::getId)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toSet());
            return new DealAccessScope(false, pipelineIds, orgIdsFromThosePipelines, false, null);
        }

        return DealAccessScope.noAccess();
    }

    public boolean canAccessPipeline(Long pipelineId, DealAccessScope scope) {
        if (scope.fullAccess()) {
            return true;
        }
        if (scope.categoryManagerCategoryScope() != null) {
            if (pipelineId == null) {
                return false;
            }
            if (scope.allowedPipelineIds().contains(pipelineId)) {
                return true;
            }
            return pipelineRepository.findById(pipelineId)
                    .filter(p -> Boolean.FALSE.equals(p.getDeleted()))
                    .filter(p -> p.getCategory() != null && scope.categoryManagerCategoryScope().categoryName() != null)
                    .map(p -> p.getCategory().trim()
                            .equalsIgnoreCase(scope.categoryManagerCategoryScope().categoryName().trim()))
                    .orElse(false);
        }
        return pipelineId != null && scope.allowedPipelineIds().contains(pipelineId);
    }

    public boolean canAccessDeal(Deal deal, DealAccessScope scope) {
        if (deal == null) {
            return false;
        }
        if (scope.fullAccess()) {
            return true;
        }
        Long pid = deal.getPipelineId();
        Long oid = deal.getOrganizationId();

        if (!scope.allowedPipelineIds().isEmpty() && pid != null && scope.allowedPipelineIds().contains(pid)) {
            return true;
        }

        DealAccessScope.CategoryManagerCategoryScope cms = scope.categoryManagerCategoryScope();
        if (cms != null) {
            if (deal.getCategoryId() != null && cms.categoryId() != null
                    && deal.getCategoryId().equals(cms.categoryId())) {
                return true;
            }
            if (oid != null && cms.categoryName() != null) {
                Optional<Organization> orgOpt = organizationRepository.findById(oid);
                if (orgOpt.isPresent()
                        && orgOpt.get().getCategory() != null
                        && orgOpt.get().getCategory().getDbValue()
                                .equalsIgnoreCase(cms.categoryName().trim())) {
                    return true;
                }
            }
            if (pid != null && cms.categoryName() != null) {
                boolean byPipe = pipelineRepository.findById(pid)
                        .filter(p -> Boolean.FALSE.equals(p.getDeleted()))
                        .filter(p -> p.getCategory() != null)
                        .map(p -> p.getCategory().trim().equalsIgnoreCase(cms.categoryName().trim()))
                        .orElse(false);
                if (byPipe) {
                    return true;
                }
            }
        }

        if (scope.matchDealsByOrganizationId()
                && oid != null
                && scope.nullablePipelineOrganizationIds().contains(oid)) {
            return true;
        }
        return pid == null && oid != null && scope.nullablePipelineOrganizationIds().contains(oid);
    }

    /**
     * Same hierarchy as {@link com.brideside.crm.service.impl.OrganizationServiceImpl} for category managers.
     */
    private List<Long> findAccessibleOwnerIdsForCategoryManager(User categoryManager) {
        List<Long> ownerIds = new ArrayList<>();
        ownerIds.add(categoryManager.getId());

        List<User> directReports = userRepository.findByManagerId(categoryManager.getId());
        for (User report : directReports) {
            if (report.getRole() != null) {
                Role.RoleName reportRole = report.getRole().getName();
                if (reportRole == Role.RoleName.SALES || reportRole == Role.RoleName.PRESALES) {
                    ownerIds.add(report.getId());
                }
            }
        }

        for (User sales : directReports) {
            if (sales.getRole() != null && sales.getRole().getName() == Role.RoleName.SALES) {
                List<User> presalesUnderSales = userRepository.findByManagerId(sales.getId());
                for (User presales : presalesUnderSales) {
                    if (presales.getRole() != null && presales.getRole().getName() == Role.RoleName.PRESALES) {
                        ownerIds.add(presales.getId());
                    }
                }
            }
        }

        return ownerIds;
    }
}
