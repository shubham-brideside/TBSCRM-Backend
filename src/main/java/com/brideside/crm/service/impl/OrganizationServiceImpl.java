package com.brideside.crm.service.impl;

import com.brideside.crm.dto.OrganizationDtos;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.OrganizationService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Transactional
public class OrganizationServiceImpl implements OrganizationService {

    private final OrganizationRepository organizationRepository;
    private final UserRepository userRepository;
    private static final Set<Role.RoleName> ALLOWED_OWNER_ROLES =
            EnumSet.of(Role.RoleName.SALES, Role.RoleName.CATEGORY_MANAGER);

    public OrganizationServiceImpl(OrganizationRepository organizationRepository,
                                   UserRepository userRepository) {
        this.organizationRepository = organizationRepository;
        this.userRepository = userRepository;
    }

    @Override
    public OrganizationDtos.OrganizationResponse create(OrganizationDtos.OrganizationRequest request) {
        Organization organization = new Organization();
        organization.setName(trimmed(request.getName()));
        organization.setOwner(resolveOwner(request.getOwnerId()));
        organization.setCategory(resolveCategory(request.getCategory()));
        organization.setAddress(trimmed(request.getAddress()));
        return toResponse(organizationRepository.save(organization));
    }

    @Override
    @Transactional(readOnly = true)
    public List<OrganizationDtos.OrganizationResponse> list() {
        return organizationRepository.findAll()
                .stream()
                .map(this::toResponse)
                .collect(Collectors.toList());
    }

    @Override
    @Transactional(readOnly = true)
    public OrganizationDtos.OrganizationResponse get(Long id) {
        return toResponse(getOrThrow(id));
    }

    @Override
    public OrganizationDtos.OrganizationResponse update(Long id, OrganizationDtos.OrganizationRequest request) {
        Organization organization = getOrThrow(id);
        organization.setName(trimmed(request.getName()));
        organization.setOwner(resolveOwner(request.getOwnerId()));
        organization.setCategory(resolveCategory(request.getCategory()));
        organization.setAddress(trimmed(request.getAddress()));
        organization.setGoogleCalendarId(trimmed(request.getGoogleCalendarId()));
        return toResponse(organizationRepository.save(organization));
    }

    @Override
    public void delete(Long id) {
        Organization organization = getOrThrow(id);
        organizationRepository.delete(organization);
    }

    @Override
    @Transactional(readOnly = true)
    public List<OrganizationDtos.OwnerOption> listOwnerOptions() {
        return userRepository.findByRole_NameInAndActiveTrue(ALLOWED_OWNER_ROLES)
                .stream()
                .map(this::toOwnerOption)
                .collect(Collectors.toList());
    }

    @Override
    @Transactional(readOnly = true)
    public List<OrganizationDtos.CategoryOption> listCategoryOptions() {
        return OrganizationDtos.allCategoryOptions();
    }

    @Override
    @Transactional(readOnly = true)
    public List<OrganizationDtos.OrganizationResponse> listAccessibleForCurrentUser(String currentUserEmail) {
        User currentUser = getCurrentUser(currentUserEmail);
        Role.RoleName roleName = currentUser.getRole() != null ? currentUser.getRole().getName() : null;

        if (roleName == null) {
            return List.of();
        }

        List<Organization> organizations;

        switch (roleName) {
            case ADMIN:
                // Admin can see all organizations
                organizations = organizationRepository.findAll();
                break;

            case CATEGORY_MANAGER:
                // Category Manager can see:
                // 1. Organizations owned by them
                // 2. Organizations owned by Sales/Presales under them
                List<Long> accessibleOwnerIds = findAccessibleOwnerIdsForCategoryManager(currentUser);
                organizations = organizationRepository.findByOwner_IdIn(accessibleOwnerIds);
                break;

            case SALES:
                // Sales can see:
                // 1. Organizations owned by them
                // 2. Organizations owned by Presales under them
                List<Long> salesAndPresalesOwnerIds = findAccessibleOwnerIdsForSales(currentUser);
                organizations = organizationRepository.findByOwner_IdIn(salesAndPresalesOwnerIds);
                break;

            case PRESALES:
                // Presales can see organizations owned by them
                organizations = organizationRepository.findByOwner_Id(currentUser.getId());
                break;

            default:
                organizations = List.of();
        }

        return organizations.stream()
                .map(this::toResponse)
                .collect(Collectors.toList());
    }

    /**
     * Find all user IDs that a Category Manager can access (for organization ownership)
     * Includes: the category manager themselves, direct Sales/Presales reports, and Sales who have Presales under them
     */
    private List<Long> findAccessibleOwnerIdsForCategoryManager(User categoryManager) {
        List<Long> ownerIds = new java.util.ArrayList<>();
        ownerIds.add(categoryManager.getId()); // Category Manager's own organizations

        // Find direct reports (Sales and Presales directly under this Category Manager)
        List<User> directReports = userRepository.findByManagerId(categoryManager.getId());
        for (User report : directReports) {
            if (report.getRole() != null) {
                Role.RoleName reportRole = report.getRole().getName();
                if (reportRole == Role.RoleName.SALES || reportRole == Role.RoleName.PRESALES) {
                    ownerIds.add(report.getId());
                }
            }
        }

        // Find Sales under this Category Manager, then find their Presales
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

    /**
     * Find all user IDs that a Sales user can access (for organization ownership)
     * Includes: the sales user themselves and their direct Presales reports
     */
    private List<Long> findAccessibleOwnerIdsForSales(User salesUser) {
        List<Long> ownerIds = new java.util.ArrayList<>();
        ownerIds.add(salesUser.getId()); // Sales user's own organizations

        // Find direct Presales reports under this Sales user
        List<User> directReports = userRepository.findByManagerId(salesUser.getId());
        for (User report : directReports) {
            if (report.getRole() != null && report.getRole().getName() == Role.RoleName.PRESALES) {
                ownerIds.add(report.getId());
            }
        }

        return ownerIds;
    }

    private User getCurrentUser(String email) {
        return userRepository.findByEmail(email)
                .orElseThrow(() -> new ResourceNotFoundException("Current user not found"));
    }

    private Organization getOrThrow(Long id) {
        return organizationRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Organization not found with id " + id));
    }

    private OrganizationDtos.OrganizationResponse toResponse(Organization organization) {
        OrganizationDtos.OrganizationResponse response = new OrganizationDtos.OrganizationResponse();
        response.setId(organization.getId());
        response.setName(organization.getName());
        response.setOwner(toOwnerSummary(organization.getOwner()));
        response.setCategory(organization.getCategory());
        response.setAddress(organization.getAddress());
        response.setGoogleCalendarId(organization.getGoogleCalendarId());
        response.setCreatedAt(organization.getCreatedAt());
        response.setUpdatedAt(organization.getUpdatedAt());
        return response;
    }

    private OrganizationDtos.OwnerSummary toOwnerSummary(User user) {
        if (user == null) {
            return null;
        }
        OrganizationDtos.OwnerSummary summary = new OrganizationDtos.OwnerSummary();
        summary.setId(user.getId());
        summary.setFirstName(user.getFirstName());
        summary.setLastName(user.getLastName());
        summary.setEmail(user.getEmail());
        summary.setRole(user.getRole() != null ? user.getRole().getName().name() : null);
        return summary;
    }

    private OrganizationDtos.OwnerOption toOwnerOption(User user) {
        OrganizationDtos.OwnerOption option = new OrganizationDtos.OwnerOption();
        option.setId(user.getId());
        option.setFirstName(user.getFirstName());
        option.setLastName(user.getLastName());
        option.setEmail(user.getEmail());
        option.setRole(user.getRole() != null ? user.getRole().getName().name() : null);
        return option;
    }

    private User resolveOwner(Long ownerId) {
        if (ownerId == null) {
            return null;
        }
        User user = userRepository.findById(ownerId)
                .orElseThrow(() -> new BadRequestException("Owner user not found with id " + ownerId));
        if (user.getRole() == null || !ALLOWED_OWNER_ROLES.contains(user.getRole().getName())) {
            throw new BadRequestException("Owner must have role SALES or CATEGORY_MANAGER");
        }
        if (Boolean.FALSE.equals(user.getActive())) {
            throw new BadRequestException("Owner must be an active user");
        }
        return user;
    }

    private Organization.OrganizationCategory resolveCategory(Organization.OrganizationCategory category) {
        if (category == null) {
            throw new BadRequestException("Organization category is required");
        }
        return category;
    }

    private String trimmed(String value) {
        return value == null ? null : value.trim();
    }
}


