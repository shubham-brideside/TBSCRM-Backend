package com.brideside.crm.service.impl;

import com.brideside.crm.dto.TargetDtos;
import com.brideside.crm.dto.TargetDtos.TargetTimePreset;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.DealSource;
import com.brideside.crm.entity.Person;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.SalesTarget;
import com.brideside.crm.entity.TargetCategory;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.exception.UnauthorizedException;
import com.brideside.crm.exception.ForbiddenException;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.repository.DealRepository;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.repository.SalesTargetRepository;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.TargetService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.YearMonth;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.EnumSet;

@Service
public class TargetServiceImpl implements TargetService {

    private final SalesTargetRepository salesTargetRepository;
    private final DealRepository dealRepository;
    private final UserRepository userRepository;
    private final OrganizationRepository organizationRepository;
    private final ZoneId zoneId = ZoneId.systemDefault();
    private static final EnumSet<TargetTimePreset> CATEGORY_PERIOD_PRESETS =
            EnumSet.of(TargetTimePreset.THIS_YEAR, TargetTimePreset.THIS_QUARTER, TargetTimePreset.HALF_YEAR, TargetTimePreset.CUSTOM_RANGE);
    private static final int MAX_PERIOD_MONTHS = 12; // Longest supported period (yearly)

    @Value("${app.targets.min-year:2025}")
    private int minTargetYear;

    public TargetServiceImpl(SalesTargetRepository salesTargetRepository,
                             DealRepository dealRepository,
                             UserRepository userRepository,
                             OrganizationRepository organizationRepository) {
        this.salesTargetRepository = salesTargetRepository;
        this.dealRepository = dealRepository;
        this.userRepository = userRepository;
        this.organizationRepository = organizationRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public TargetDtos.TargetResponse get(Long id) {
        SalesTarget target = salesTargetRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Target not found with id " + id));
        return toResponse(target, getCurrentUserRole());
    }

    @Override
    @Transactional
    public TargetDtos.TargetResponse create(TargetDtos.TargetUpsertRequest request) {
        validateUpsertRequest(request);
        TargetCategory category = requireCategory(request.category);
        SalesTarget.PeriodType periodType = resolvePeriodType(request.periodType);
        LocalDate periodStart = resolvePeriodStart(request, periodType);
        ensureYearAllowed(periodStart.getYear());

        User user = userRepository.findById(request.userId)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with id " + request.userId));

        // Validate user is allowed for targets (SALES or PRESALES)
        Role.RoleName roleName = user.getRole() != null ? user.getRole().getName() : null;
        if (roleName != Role.RoleName.SALES && roleName != Role.RoleName.PRESALES) {
            throw new BadRequestException("Targets can only be set for SALES or PRESALES users");
        }

        // Check for duplicate target
        salesTargetRepository.findByUser_IdAndCategoryAndPeriodTypeAndPeriodStart(
                user.getId(), category, periodType, periodStart)
                .ifPresent(existing -> {
                    throw new BadRequestException("Target already exists for user, category and period. Use update instead.");
                });

        SalesTarget target = new SalesTarget();
        target.setUser(user);
        target.setCategory(category);
        target.setPeriodType(periodType);
        target.setPeriodStart(periodStart);
        target.setTargetAmount(request.targetAmount.setScale(2, RoundingMode.HALF_UP));

        // Set organizations if provided
        if (request.organizationIds != null && !request.organizationIds.isEmpty()) {
            Set<Organization> organizations = resolveOrganizations(
                    request.organizationIds, user, Collections.emptySet());
            target.setOrganizations(organizations);
        }

        SalesTarget saved = salesTargetRepository.save(target);
        return toResponse(saved, getCurrentUserRole());
    }

    @Override
    @Transactional
    public TargetDtos.TargetResponse update(Long id, TargetDtos.TargetUpsertRequest request) {
        validateUpsertRequest(request);
        SalesTarget target = salesTargetRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Target not found with id " + id));

        TargetCategory category = requireCategory(request.category);
        SalesTarget.PeriodType periodType = resolvePeriodType(request.periodType);
        LocalDate periodStart = resolvePeriodStart(request, periodType);
        ensureYearAllowed(periodStart.getYear());

        if (!target.getUser().getId().equals(request.userId)) {
            User user = userRepository.findById(request.userId)
                    .orElseThrow(() -> new ResourceNotFoundException("User not found with id " + request.userId));
            // Validate user is allowed for targets (SALES or PRESALES)
            Role.RoleName roleName = user.getRole() != null ? user.getRole().getName() : null;
            if (roleName != Role.RoleName.SALES && roleName != Role.RoleName.PRESALES) {
                throw new BadRequestException("Targets can only be set for SALES or PRESALES users");
            }
            target.setUser(user);
        }

        // Check for duplicate target
        Optional<SalesTarget> duplicate = salesTargetRepository
                .findByUser_IdAndCategoryAndPeriodTypeAndPeriodStart(
                        target.getUser().getId(), category, periodType, periodStart);
        if (duplicate.isPresent() && !duplicate.get().getId().equals(target.getId())) {
            throw new BadRequestException("Another target already exists for this user/category/period.");
        }

        target.setCategory(category);
        target.setPeriodType(periodType);
        target.setPeriodStart(periodStart);
        target.setTargetAmount(request.targetAmount.setScale(2, RoundingMode.HALF_UP));

        // Update organizations if provided
        if (request.organizationIds != null) {
            Set<Organization> organizations = request.organizationIds.isEmpty()
                    ? new HashSet<>()
                    : resolveOrganizations(request.organizationIds, target.getUser(),
                            target.getOrganizations());
            target.setOrganizations(organizations);
        }

        SalesTarget saved = salesTargetRepository.save(target);
        return toResponse(saved, getCurrentUserRole());
    }

    @Override
    @Transactional
    public void delete(Long id) {
        SalesTarget target = salesTargetRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Target not found with id " + id));
        salesTargetRepository.delete(target);
    }

    @Override
    @Transactional(readOnly = true)
    public List<TargetDtos.TargetResponse> list(TargetDtos.TargetListFilter filter) {
        TargetDtos.TargetListFilter safeFilter = filter != null ? filter : new TargetDtos.TargetListFilter();
        YearMonth yearMonth = resolveListYearMonth(safeFilter);

        // For list, we'll show all targets that overlap with the specified month
        // This includes monthly targets for that month, quarterly/half-yearly/yearly that include that month
        LocalDate monthStart = yearMonth.atDay(1);
        LocalDate monthEnd = yearMonth.atEndOfMonth();
        
        List<SalesTarget> targets = salesTargetRepository.findByPeriodStartBetween(monthStart, monthEnd);
        
        // Filter to only include targets that actually cover the requested month
        targets = targets.stream()
                .filter(target -> {
                    YearMonth targetStart = YearMonth.from(target.getPeriodStart());
                    if (target.getPeriodType() == SalesTarget.PeriodType.MONTHLY) {
                        return targetStart.equals(yearMonth);
                    } else if (target.getPeriodType() == SalesTarget.PeriodType.QUARTERLY) {
                        int quarterStartMonth = ((targetStart.getMonthValue() - 1) / 3) * 3 + 1;
                        YearMonth quarterStart = YearMonth.of(targetStart.getYear(), quarterStartMonth);
                        YearMonth quarterEnd = quarterStart.plusMonths(2);
                        return !yearMonth.isBefore(quarterStart) && !yearMonth.isAfter(quarterEnd);
                    } else if (target.getPeriodType() == SalesTarget.PeriodType.HALF_YEARLY) {
                        int halfYearStartMonth = targetStart.getMonthValue() <= 6 ? 1 : 7;
                        YearMonth halfYearStart = YearMonth.of(targetStart.getYear(), halfYearStartMonth);
                        YearMonth halfYearEnd = halfYearStart.plusMonths(5);
                        return !yearMonth.isBefore(halfYearStart) && !yearMonth.isAfter(halfYearEnd);
                    } else if (target.getPeriodType() == SalesTarget.PeriodType.YEARLY) {
                        return targetStart.getYear() == yearMonth.getYear();
                    }
                    return false;
                })
                .collect(Collectors.toList());
        
        TargetCategory category = TargetCategory.fromValue(safeFilter.category);
        if (category != null) {
            targets = targets.stream()
                    .filter(t -> t.getCategory() == category)
                    .collect(Collectors.toList());
        }

        Role.RoleName role = getCurrentUserRole();
        return targets.stream()
                .map(target -> toResponse(target, role))
                .sorted(Comparator.comparing(t -> t.userName == null ? "" : t.userName.toLowerCase(Locale.ROOT)))
                .collect(Collectors.toList());
    }

    @Override
    @Transactional(readOnly = true)
    public TargetDtos.DashboardResponse dashboard(TargetDtos.DashboardFilter filter) {
        TargetDtos.DashboardFilter safeFilter = filter != null ? filter : new TargetDtos.DashboardFilter();
        if (safeFilter.preset == null) {
            safeFilter.preset = inferPreset(safeFilter);
        }

        Role.RoleName role = getCurrentUserRole();
        DashboardComputation computation = buildDashboardComputation(safeFilter);

        List<TargetCategory> categories = computation.categoryFilter != null
                ? List.of(computation.categoryFilter)
                : Arrays.asList(TargetCategory.values());

        List<TargetDtos.MonthBlock> monthBlocks = new ArrayList<>();
        for (YearMonth ym : computation.months) {
            TargetDtos.MonthBlock block = new TargetDtos.MonthBlock();
            block.month = ym.getMonthValue();
            block.year = ym.getYear();
            block.editable = isEditableForRole(ym, role);
            block.categories = new ArrayList<>();

            for (TargetCategory category : categories) {
                TargetDtos.CategoryTable table = new TargetDtos.CategoryTable();
                table.category = category.getCode();
                table.categoryLabel = category.getLabel();

                List<SalesTarget> categoryTargets = computation.targetsByMonth
                        .getOrDefault(ym, Collections.emptyMap())
                        .getOrDefault(category, Collections.emptyList());

                List<TargetDtos.TargetRow> rows = new ArrayList<>();
                for (SalesTarget categoryTarget : categoryTargets) {
                    rows.add(toTargetRow(
                            categoryTarget,
                            computation.achievedMap,
                            computation.presalesAllCategoryMap,
                            ym,
                            category));
                }
                table.rows = rows;
                block.categories.add(table);
            }

            monthBlocks.add(block);
        }

        TargetDtos.DashboardResponse response = new TargetDtos.DashboardResponse();
        response.filters = buildAppliedFilters(safeFilter, computation.months, role);
        response.months = monthBlocks;
        response.deals = computation.dealSummaries;
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public TargetDtos.CategoryMonthlyBreakdownResponse categoryMonthlyBreakdown(TargetDtos.DashboardFilter filter) {
        TargetDtos.DashboardFilter safeFilter = filter != null ? filter : new TargetDtos.DashboardFilter();
        if (safeFilter.category == null) {
            throw new BadRequestException("category is required");
        }
        if (safeFilter.preset == null) {
            safeFilter.preset = inferPreset(safeFilter);
        }
        if (!CATEGORY_PERIOD_PRESETS.contains(safeFilter.preset)) {
            throw new BadRequestException("timePreset must be one of " + CATEGORY_PERIOD_PRESETS);
        }

        Role.RoleName role = getCurrentUserRole();
        DashboardComputation computation = buildDashboardComputation(safeFilter);

        TargetDtos.CategoryMonthlyBreakdownResponse response = new TargetDtos.CategoryMonthlyBreakdownResponse();
        response.category = safeFilter.category.getCode();
        response.categoryLabel = safeFilter.category.getLabel();
        response.filters = buildAppliedFilters(safeFilter, computation.months, role);

        List<TargetDtos.CategoryMonthlyRow> monthRows = new ArrayList<>();
        BigDecimal totalTarget = BigDecimal.ZERO;
        BigDecimal totalAchieved = BigDecimal.ZERO;
        int totalDeals = 0;

        for (YearMonth ym : computation.months) {
            List<SalesTarget> categoryTargets = computation.targetsByMonth
                    .getOrDefault(ym, Collections.emptyMap())
                    .getOrDefault(safeFilter.category, Collections.emptyList());
            List<TargetDtos.TargetRow> users = categoryTargets.stream()
                    .map(target -> toTargetRow(
                            target,
                            computation.achievedMap,
                            computation.presalesAllCategoryMap,
                            ym,
                            safeFilter.category))
                    .collect(Collectors.toList());

            BigDecimal monthTarget = users.stream()
                    .map(row -> safeValue(row.totalTarget))
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
            BigDecimal monthAchieved = users.stream()
                    .map(row -> safeValue(row.achieved))
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
            int monthDeals = users.stream()
                    .map(row -> row.totalDeals != null ? row.totalDeals : 0)
                    .reduce(0, Integer::sum);

            BigDecimal achievementPercent = calculateAchievementPercent(monthAchieved, monthTarget);
            BigDecimal incentivePercent = calculateIncentive(achievementPercent);
            BigDecimal incentiveAmount = monthAchieved.compareTo(BigDecimal.ZERO) == 0
                    ? BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP)
                    : incentivePercent.multiply(monthAchieved)
                    .divide(BigDecimal.valueOf(100), 2, RoundingMode.HALF_UP);

            TargetDtos.CategoryMonthlyRow row = new TargetDtos.CategoryMonthlyRow();
            row.month = ym.getMonthValue();
            row.year = ym.getYear();
            row.totalTarget = monthTarget.setScale(2, RoundingMode.HALF_UP);
            row.achieved = monthAchieved.setScale(2, RoundingMode.HALF_UP);
            row.achievementPercent = achievementPercent;
            row.totalDeals = monthDeals;
            row.incentivePercent = incentivePercent;
            row.incentiveAmount = incentiveAmount;
            row.users = users;
            monthRows.add(row);

            totalTarget = totalTarget.add(monthTarget);
            totalAchieved = totalAchieved.add(monthAchieved);
            totalDeals += monthDeals;
        }

        TargetDtos.CategoryMonthlyTotals totals = new TargetDtos.CategoryMonthlyTotals();
        totals.totalTarget = totalTarget.setScale(2, RoundingMode.HALF_UP);
        totals.achieved = totalAchieved.setScale(2, RoundingMode.HALF_UP);
        totals.totalDeals = totalDeals;
        totals.achievementPercent = calculateAchievementPercent(totalAchieved, totalTarget);
        totals.incentivePercent = calculateIncentive(totals.achievementPercent);
        totals.incentiveAmount = totalAchieved.compareTo(BigDecimal.ZERO) == 0
                ? BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP)
                : totals.incentivePercent.multiply(totalAchieved)
                .divide(BigDecimal.valueOf(100), 2, RoundingMode.HALF_UP);

        response.months = monthRows;
        response.totals = totals;
        return response;
    }

    private DashboardComputation buildDashboardComputation(TargetDtos.DashboardFilter filter) {
        List<YearMonth> months = resolveMonths(filter);
        if (months.isEmpty()) {
            months = List.of(YearMonth.now(zoneId));
        }

        // Get the current logged-in user to filter targets and deals
        User currentUser = requireCurrentUser();
        Long currentUserId = currentUser.getId();
        Role.RoleName currentUserRole = currentUser.getRole() != null 
                ? currentUser.getRole().getName() 
                : null;

        TargetCategory categoryFilter = filter.category;
        LocalDate startDate = months.get(0).atDay(1);
        LocalDate endDate = months.get(months.size() - 1).atEndOfMonth();
        LocalDate queryStart = resolveTargetQueryStart(startDate);

        // Filter targets to only include those for the current logged-in user and their team
        List<SalesTarget> allTargets = categoryFilter != null
                ? salesTargetRepository.findByCategoryAndPeriodStartBetween(categoryFilter, queryStart, endDate)
                : salesTargetRepository.findByPeriodStartBetween(queryStart, endDate);
        
        // Get accessible users based on role hierarchy
        Set<Long> accessibleUserIds = new HashSet<>();
        accessibleUserIds.add(currentUserId);
        
        if (currentUserRole == Role.RoleName.ADMIN) {
            // ADMIN can see all SALES and PRESALES users across all categories
            List<User> allActiveSalesAndPresales = userRepository.findByRole_NameInAndActiveTrue(
                    Arrays.asList(Role.RoleName.SALES, Role.RoleName.PRESALES));
            for (User user : allActiveSalesAndPresales) {
                if (user.getId() != null) {
                    accessibleUserIds.add(user.getId());
                }
            }
        } else if (currentUserRole == Role.RoleName.CATEGORY_MANAGER) {
            // CATEGORY_MANAGER can see all SALES and PRESALES users under them
            List<User> allActiveSalesAndPresales = userRepository.findByRole_NameInAndActiveTrue(
                    Arrays.asList(Role.RoleName.SALES, Role.RoleName.PRESALES));
            for (User user : allActiveSalesAndPresales) {
                if (isAccessibleForTargets(currentUser, user, currentUserRole)) {
                    if (user.getId() != null) {
                        accessibleUserIds.add(user.getId());
                    }
                }
            }
        } else if (currentUserRole == Role.RoleName.SALES) {
            // SALES users can see their PRESALES team members' targets
            List<User> allSubordinates = userRepository.findByManagerId(currentUserId);
            List<User> presalesTeam = allSubordinates.stream()
                    .filter(u -> u.getActive() != null && u.getActive()
                            && u.getRole() != null 
                            && u.getRole().getName() == Role.RoleName.PRESALES)
                    .collect(Collectors.toList());
            for (User presalesUser : presalesTeam) {
                if (presalesUser.getId() != null) {
                    accessibleUserIds.add(presalesUser.getId());
                }
            }
        } else if (currentUserRole == Role.RoleName.PRESALES 
                && currentUser.getManager() != null 
                && currentUser.getManager().getId() != null) {
            // PRESALES users can see their manager's targets
            accessibleUserIds.add(currentUser.getManager().getId());
        }
        
        // Filter to only show targets for accessible users
        final Set<Long> finalAccessibleUserIds = accessibleUserIds;
        List<SalesTarget> targets = allTargets.stream()
                .filter(target -> {
                    if (target.getUser() == null || target.getUser().getId() == null) {
                        return false;
                    }
                    Long targetUserId = target.getUser().getId();
                    return finalAccessibleUserIds.contains(targetUserId);
                })
                .collect(Collectors.toList());

        Map<YearMonth, Map<TargetCategory, List<SalesTarget>>> targetsByMonth = new HashMap<>();
        for (SalesTarget target : targets) {
            YearMonth targetStart = YearMonth.from(target.getPeriodStart());
            List<YearMonth> coveredMonths = new ArrayList<>();
            if (target.getPeriodType() == SalesTarget.PeriodType.MONTHLY) {
                coveredMonths.add(targetStart);
            } else if (target.getPeriodType() == SalesTarget.PeriodType.QUARTERLY) {
                for (int i = 0; i < 3; i++) {
                    coveredMonths.add(targetStart.plusMonths(i));
                }
            } else if (target.getPeriodType() == SalesTarget.PeriodType.HALF_YEARLY) {
                for (int i = 0; i < 6; i++) {
                    coveredMonths.add(targetStart.plusMonths(i));
                }
            } else if (target.getPeriodType() == SalesTarget.PeriodType.YEARLY) {
                for (int i = 0; i < 12; i++) {
                    coveredMonths.add(targetStart.plusMonths(i));
                }
            }
            
            for (YearMonth ym : coveredMonths) {
                if (months.contains(ym)) {
                    targetsByMonth
                            .computeIfAbsent(ym, key -> new EnumMap<>(TargetCategory.class))
                            .computeIfAbsent(target.getCategory(), key -> new ArrayList<>())
                            .add(target);
                }
            }
        }

        LocalDateTime dealsStart = startDate.atStartOfDay();
        LocalDateTime dealsEnd = endDate.plusDays(1).atStartOfDay();
        List<Deal> allWonDeals = dealRepository.findWonDealsUpdatedBetween(dealsStart, dealsEnd);
        
        // Filter deals to only include those related to accessible users (current user + their team)
        final Set<Long> finalAccessibleUserIdsForDeals = accessibleUserIds;
        List<Deal> wonDeals = allWonDeals.stream()
                .filter(deal -> {
                    User owner = resolveDealOwner(deal);
                    if (owner == null || owner.getId() == null) {
                        return false;
                    }
                    Long ownerId = owner.getId();
                    return finalAccessibleUserIdsForDeals.contains(ownerId);
                })
                .collect(Collectors.toList());
        
        wonDeals.sort(Comparator.comparing(
                (Deal d) -> d.getUpdatedAt() != null ? d.getUpdatedAt() : d.getCreatedAt(),
                Comparator.nullsLast(Comparator.naturalOrder())
        ).reversed());

        // Pre-compute active PRESALES users grouped by their SALES manager so
        // that diversion / deal counts can be mirrored onto Pre‑Sales target
        // rows across all dashboard tables (main dashboard + category
        // breakdowns). This keeps Pre‑Sales Achieved / Total Deals aligned
        // with the diverted deals they help close, similar to the per-user
        // logic in getUserMonthlyDetail.
        // Only include PRESALES users related to the current user
        Map<Long, List<User>> presalesByManagerId = new HashMap<>();
        if (currentUserRole == Role.RoleName.ADMIN) {
            // For ADMIN, include all PRESALES users, grouped by their SALES managers
            List<User> allPresales = userRepository.findByRole_NameAndActiveTrue(Role.RoleName.PRESALES);
            for (User presalesUser : allPresales) {
                if (presalesUser.getManager() != null 
                        && presalesUser.getManager().getId() != null) {
                    Long managerId = presalesUser.getManager().getId();
                    presalesByManagerId.computeIfAbsent(managerId, k -> new ArrayList<>()).add(presalesUser);
                }
            }
        } else if (currentUserRole == Role.RoleName.CATEGORY_MANAGER) {
            // For CATEGORY_MANAGER, include all PRESALES users under them, grouped by their SALES managers
            List<User> allActiveSalesAndPresales = userRepository.findByRole_NameInAndActiveTrue(
                    Arrays.asList(Role.RoleName.SALES, Role.RoleName.PRESALES));
            for (User user : allActiveSalesAndPresales) {
                if (user.getRole() != null 
                        && user.getRole().getName() == Role.RoleName.PRESALES
                        && isAccessibleForTargets(currentUser, user, currentUserRole)
                        && user.getManager() != null 
                        && user.getManager().getId() != null) {
                    Long managerId = user.getManager().getId();
                    presalesByManagerId.computeIfAbsent(managerId, k -> new ArrayList<>()).add(user);
                }
            }
        } else if (currentUserRole == Role.RoleName.SALES) {
            // For SALES users, include their PRESALES team members
            List<User> allSubordinates = userRepository.findByManagerId(currentUserId);
            List<User> presalesTeam = allSubordinates.stream()
                    .filter(u -> u.getActive() != null && u.getActive()
                            && u.getRole() != null 
                            && u.getRole().getName() == Role.RoleName.PRESALES)
                    .collect(Collectors.toList());
            if (!presalesTeam.isEmpty()) {
                presalesByManagerId.put(currentUserId, presalesTeam);
            }
        } else if (currentUserRole == Role.RoleName.PRESALES 
                && currentUser.getManager() != null 
                && currentUser.getManager().getId() != null) {
            // For PRESALES users, only include themselves under their manager
            presalesByManagerId.put(currentUser.getManager().getId(), List.of(currentUser));
        }

        Map<YearMonth, Map<TargetCategory, Map<Long, DealAggregate>>> achievedMap = new HashMap<>();
        // For PRESALES users we also need deal aggregates that are independent
        // of category so that the dashboard mirrors the same Achieved / Deals
        // behaviour as the per‑user detail API. This map tracks, per month,
        // all won deals (all sources, all categories) attributed to each user.
        Map<YearMonth, Map<Long, DealAggregate>> presalesAllCategoryMap = new HashMap<>();
        List<TargetDtos.DealSummary> dealSummaries = new ArrayList<>();

        for (Deal deal : wonDeals) {
            LocalDateTime reference = deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt();
            if (reference == null) {
                continue;
            }
            YearMonth dealMonth = YearMonth.from(reference);
            if (!months.contains(dealMonth)) {
                continue;
            }

            User owner = resolveDealOwner(deal);
            if (owner != null && owner.getId() != null) {
                Long ownerId = owner.getId();
                // Only process deals for accessible users (already filtered above, but double-check)
                if (!finalAccessibleUserIdsForDeals.contains(ownerId)) {
                    continue;
                }
                
                TargetCategory dealCategory = TargetCategory.fromDeal(deal);
                if (dealCategory != null) {
                    // Category‑scoped aggregates used for SALES rows and
                    // category‑specific breakdowns. These respect the
                    // dashboard's category filter.
                    if (categoryFilter == null || categoryFilter == dealCategory) {
                        DealAggregate aggregate = achievedMap
                                .computeIfAbsent(dealMonth, key -> new EnumMap<>(TargetCategory.class))
                                .computeIfAbsent(dealCategory, key -> new HashMap<>())
                                .computeIfAbsent(ownerId, key -> new DealAggregate());
                        aggregate.achieved = aggregate.achieved.add(safeValue(deal.getValue()));
                        aggregate.dealsCount += 1;

                        // A deal is considered "diverted" if either isDiverted flag is true OR dealSource is DIVERT
                        boolean diverted = Boolean.TRUE.equals(deal.getIsDiverted()) 
                                || (deal.getDealSource() != null && deal.getDealSource() == DealSource.DIVERT);
                        if (diverted) {
                            aggregate.diversionDeals += 1;
                        } else {
                            aggregate.directDeals += 1;
                        }
                    }
                }

                // Category‑agnostic aggregates for PRESALES rows. These match
                // the detail API behaviour by counting all won deals (all
                // sources and categories) under the linked SALES manager and
                // mirroring them to each PRESALES user.
                // Owner entry (SALES) – primarily useful if we ever want a
                // unified per‑user view; harmless for now.
                DealAggregate ownerAllAggregate = presalesAllCategoryMap
                        .computeIfAbsent(dealMonth, key -> new HashMap<>())
                        .computeIfAbsent(ownerId, key -> new DealAggregate());
                ownerAllAggregate.achieved = ownerAllAggregate.achieved.add(safeValue(deal.getValue()));
                ownerAllAggregate.dealsCount += 1;
                // A deal is considered "diverted" if either isDiverted flag is true OR dealSource is DIVERT
                boolean diverted = Boolean.TRUE.equals(deal.getIsDiverted()) 
                        || (deal.getDealSource() != null && deal.getDealSource() == DealSource.DIVERT);
                if (diverted) {
                    ownerAllAggregate.diversionDeals += 1;
                } else {
                    ownerAllAggregate.directDeals += 1;
                }

                // Mirror onto all PRESALES users reporting to this SALES owner.
                List<User> presalesTeam = presalesByManagerId.get(ownerId);
                if (presalesTeam != null && !presalesTeam.isEmpty()) {
                    for (User presalesUser : presalesTeam) {
                        Long presalesId = presalesUser.getId();
                        if (presalesId == null) {
                            continue;
                        }
                        DealAggregate presalesAggregate = presalesAllCategoryMap
                                .computeIfAbsent(dealMonth, key -> new HashMap<>())
                                .computeIfAbsent(presalesId, key -> new DealAggregate());
                        presalesAggregate.achieved = presalesAggregate.achieved.add(safeValue(deal.getValue()));
                        presalesAggregate.dealsCount += 1;
                        if (diverted) {
                            presalesAggregate.diversionDeals += 1;
                        } else {
                            presalesAggregate.directDeals += 1;
                        }
                    }
                }
            }

            // For the won deals table we still tag each deal with its
            // category (when available) using the same helper as before.
            // For PRESALES users, attribute deals to the PRESALES user instead of the manager
            TargetCategory summaryCategory = TargetCategory.fromDeal(deal);
            User summaryUser = owner;
            if (currentUserRole == Role.RoleName.PRESALES 
                    && currentUser.getManager() != null 
                    && currentUser.getManager().getId() != null
                    && owner != null 
                    && owner.getId() != null
                    && owner.getId().equals(currentUser.getManager().getId())) {
                // Attribute manager's deals to the PRESALES user
                summaryUser = currentUser;
            }
            dealSummaries.add(toDealSummary(deal, summaryUser, summaryCategory));
        }

        DashboardComputation computation = new DashboardComputation();
        computation.months = months;
        computation.targetsByMonth = targetsByMonth;
        computation.achievedMap = achievedMap;
        computation.presalesAllCategoryMap = presalesAllCategoryMap;
        computation.dealSummaries = dealSummaries;
        computation.categoryFilter = categoryFilter;
        return computation;
    }

    @Override
    @Transactional(readOnly = true)
    public TargetDtos.FiltersResponse filters() {
        TargetDtos.FiltersResponse response = new TargetDtos.FiltersResponse();
        User currentUser = requireCurrentUser();
        Role.RoleName currentRole = currentUser.getRole() != null
                ? currentUser.getRole().getName()
                : null;

        // --- Category options (role-aware default) ---
        List<TargetCategory> allCategories = Arrays.asList(TargetCategory.values());
        List<TargetCategory> scopedCategories;

        if (currentRole == Role.RoleName.ADMIN) {
            // Admin can always see all categories.
            scopedCategories = allCategories;
        } else {
            // For non-admins, try to infer categories from organizations they own.
            List<Organization> ownedOrganizations = organizationRepository.findByOwner_Id(currentUser.getId());
            Set<TargetCategory> inferred = ownedOrganizations.stream()
                    .map(Organization::getCategory)
                    .filter(Objects::nonNull)
                    .map(cat -> TargetCategory.fromValue(cat.getDbValue()))
                    .filter(Objects::nonNull)
                    .collect(Collectors.toCollection(() -> EnumSet.noneOf(TargetCategory.class)));

            if (inferred.isEmpty()) {
                // Fallback: if we can't infer anything yet (e.g. new user with no orgs),
                // keep behaviour same as before and expose all categories.
                scopedCategories = allCategories;
            } else {
                scopedCategories = new ArrayList<>(inferred);
                scopedCategories.sort(Comparator.comparing(TargetCategory::getLabel));
            }
        }

        response.categories = scopedCategories.stream()
                .map(category -> {
                    TargetDtos.FilterOption option = new TargetDtos.FilterOption();
                    option.code = category.getCode();
                    option.label = category.getLabel();
                    return option;
                })
                .collect(Collectors.toList());

        response.presets = Arrays.stream(TargetTimePreset.values())
                .map(preset -> {
                    TargetDtos.FilterOption option = new TargetDtos.FilterOption();
                    option.code = preset.name();
                    option.label = preset.getLabel();
                    return option;
                })
                .collect(Collectors.toList());

        response.minYear = minTargetYear;

        // --- User options (role-aware "Select Users" dropdown) ---
        List<User> accessibleUsers = resolveTargetFilterUsers(currentUser);
        List<TargetDtos.UserFilterOption> userOptions = accessibleUsers.stream()
                .map(user -> {
                    TargetDtos.UserFilterOption option = new TargetDtos.UserFilterOption();
                    option.id = user.getId();
                    option.name = ((user.getFirstName() != null ? user.getFirstName() : "") + " "
                            + (user.getLastName() != null ? user.getLastName() : "")).trim();
                    option.role = user.getRole() != null ? user.getRole().getName().name() : null;
                    option.currentUser = Objects.equals(user.getId(), currentUser.getId());
                    return option;
                })
                .sorted(Comparator.comparing(opt -> opt.name.toLowerCase(Locale.ROOT)))
                .collect(Collectors.toList());

        response.users = userOptions;
        response.currentUserId = currentUser.getId();
        response.currentUserName = ((currentUser.getFirstName() != null ? currentUser.getFirstName() : "") + " "
                + (currentUser.getLastName() != null ? currentUser.getLastName() : "")).trim();
        response.currentUserRole = currentRole != null ? currentRole.name() : null;

        if (!scopedCategories.isEmpty() && currentRole != Role.RoleName.ADMIN) {
            TargetCategory defaultCat = scopedCategories.get(0);
            response.defaultCategoryCode = defaultCat.getCode();
            response.defaultCategoryLabel = defaultCat.getLabel();
        }

        return response;
    }

    /**
     * Resolve which users should appear in the Target dashboard "Select Users"
     * dropdown for the current user. This mirrors the user access hierarchy:
     * - ADMIN: all active SALES + PRESALES users
     * - CATEGORY_MANAGER: self + SALES/PRESALES under them
     * - SALES: self + PRESALES under them
     * - PRESALES: self only
     */
    private List<User> resolveTargetFilterUsers(User currentUser) {
        Role.RoleName role = currentUser.getRole() != null
                ? currentUser.getRole().getName()
                : null;

        if (role == null) {
            return List.of(currentUser);
        }

        if (role == Role.RoleName.ADMIN) {
            // Admin sees all active sales + presales users
            return userRepository.findByRole_NameInAndActiveTrue(
                    Arrays.asList(Role.RoleName.SALES, Role.RoleName.PRESALES));
        }

        if (role == Role.RoleName.PRESALES) {
            // Presales sees only themselves
            return List.of(currentUser);
        }

        // For CATEGORY_MANAGER and SALES we need to filter based on hierarchy.
        List<User> allActiveSalesAndPresales = userRepository.findByRole_NameInAndActiveTrue(
                Arrays.asList(Role.RoleName.SALES, Role.RoleName.PRESALES));

        return allActiveSalesAndPresales.stream()
                .filter(user -> isAccessibleForTargets(currentUser, user, role))
                .collect(Collectors.toList());
    }

    /**
     * Minimal copy of the user hierarchy rules for the Target page.
     */
    private boolean isAccessibleForTargets(User currentUser, User targetUser, Role.RoleName currentRole) {
        Role.RoleName targetRole = targetUser.getRole() != null
                ? targetUser.getRole().getName()
                : null;

        // Users can always see themselves
        if (Objects.equals(currentUser.getId(), targetUser.getId())) {
            return true;
        }

        if (currentRole == Role.RoleName.CATEGORY_MANAGER) {
            if (targetRole == Role.RoleName.SALES || targetRole == Role.RoleName.PRESALES) {
                return isUnderUser(currentUser, targetUser);
            }
            return false;
        }

        if (currentRole == Role.RoleName.SALES) {
            if (targetRole == Role.RoleName.PRESALES) {
                return isUnderUser(currentUser, targetUser);
            }
            // Sales should always see themselves (handled above).
            return false;
        }

        // Fallback: only self
        return Objects.equals(currentUser.getId(), targetUser.getId());
    }

    /**
     * Check if target user is under current user in the manager hierarchy.
     * This mirrors the helper used in {@link com.brideside.crm.service.impl.UserServiceImpl}.
     */
    private boolean isUnderUser(User currentUser, User targetUser) {
        // Direct manager relationship
        if (targetUser.getManager() != null
                && Objects.equals(targetUser.getManager().getId(), currentUser.getId())) {
            return true;
        }

        if (currentUser.getRole() != null
                && currentUser.getRole().getName() == Role.RoleName.CATEGORY_MANAGER) {
            if (targetUser.getManager() != null) {
                User targetManager = targetUser.getManager();
                if (targetManager.getRole() != null
                        && targetManager.getRole().getName() == Role.RoleName.SALES
                        && targetManager.getManager() != null
                        && Objects.equals(targetManager.getManager().getId(), currentUser.getId())) {
                    return true;
                }
            }
        }

        return false;
    }

    private void validateUpsertRequest(TargetDtos.TargetUpsertRequest request) {
        if (request == null) {
            throw new BadRequestException("Request body is required");
        }
        if (request.userId == null) {
            throw new BadRequestException("userId is required");
        }
        if (request.category == null || request.category.isBlank()) {
            throw new BadRequestException("category is required");
        }
        if (request.periodType == null || request.periodType.isBlank()) {
            throw new BadRequestException("periodType is required (MONTHLY, QUARTERLY, HALF_YEARLY, YEARLY)");
        }
        if (request.year == null) {
            throw new BadRequestException("year is required");
        }
        if (request.year < minTargetYear) {
            throw new BadRequestException("year must be greater than or equal to " + minTargetYear);
        }
        
        // Validate period-specific fields
        SalesTarget.PeriodType periodType = resolvePeriodType(request.periodType);
        if (periodType == SalesTarget.PeriodType.MONTHLY || 
            periodType == SalesTarget.PeriodType.QUARTERLY || 
            periodType == SalesTarget.PeriodType.HALF_YEARLY) {
            if (request.month == null || request.month < 1 || request.month > 12) {
                throw new BadRequestException("month must be between 1 and 12");
            }
        }
        if (periodType == SalesTarget.PeriodType.QUARTERLY) {
            if (request.quarter == null || request.quarter < 1 || request.quarter > 4) {
                throw new BadRequestException("quarter must be between 1 and 4");
            }
        }
        if (periodType == SalesTarget.PeriodType.HALF_YEARLY) {
            if (request.halfYear == null || (request.halfYear != 1 && request.halfYear != 2)) {
                throw new BadRequestException("halfYear must be 1 or 2");
            }
        }
        
        if (request.targetAmount == null || request.targetAmount.compareTo(BigDecimal.ZERO) < 0) {
            throw new BadRequestException("targetAmount must be zero or positive");
        }
    }

    private TargetCategory requireCategory(String raw) {
        TargetCategory category = TargetCategory.fromValue(raw);
        if (category == null) {
            throw new BadRequestException("Unknown category: " + raw);
        }
        return category;
    }

    private SalesTarget.PeriodType resolvePeriodType(String raw) {
        if (raw == null || raw.isBlank()) {
            return SalesTarget.PeriodType.MONTHLY; // Default
        }
        try {
            return SalesTarget.PeriodType.valueOf(raw.trim().toUpperCase());
        } catch (IllegalArgumentException ex) {
            throw new BadRequestException("Invalid periodType: " + raw + ". Must be MONTHLY, QUARTERLY, HALF_YEARLY, or YEARLY");
        }
    }

    private LocalDate resolvePeriodStart(TargetDtos.TargetUpsertRequest request, SalesTarget.PeriodType periodType) {
        int year = request.year;
        
        switch (periodType) {
            case MONTHLY:
                if (request.month == null) {
                    throw new BadRequestException("month is required for MONTHLY period");
                }
                return YearMonth.of(year, request.month).atDay(1);
                
            case QUARTERLY:
                if (request.quarter == null) {
                    throw new BadRequestException("quarter is required for QUARTERLY period");
                }
                int quarterStartMonth = (request.quarter - 1) * 3 + 1;
                return YearMonth.of(year, quarterStartMonth).atDay(1);
                
            case HALF_YEARLY:
                if (request.halfYear == null) {
                    throw new BadRequestException("halfYear is required for HALF_YEARLY period");
                }
                int halfYearStartMonth = request.halfYear == 1 ? 1 : 7;
                return YearMonth.of(year, halfYearStartMonth).atDay(1);
                
            case YEARLY:
                return YearMonth.of(year, 1).atDay(1);
                
            default:
                throw new BadRequestException("Unsupported period type: " + periodType);
        }
    }

    private Set<Organization> resolveOrganizations(
            List<Long> organizationIds,
            User salesUser,
            Set<Organization> existingAssignments) {
        if (organizationIds == null || organizationIds.isEmpty()) {
            return new HashSet<>();
        }
        
        Set<Long> uniqueIds = new HashSet<>(organizationIds);
        List<Organization> organizations = organizationRepository.findAllById(uniqueIds);
        
        if (organizations.size() != uniqueIds.size()) {
            throw new BadRequestException("One or more organization IDs are invalid");
        }
        
        Set<Long> grandfatheredIds = existingAssignments == null
                ? Collections.emptySet()
                : existingAssignments.stream()
                    .filter(Objects::nonNull)
                    .map(Organization::getId)
                    .collect(Collectors.toSet());
        
        // Determine role of the target owner user (SALES vs PRESALES, etc.)
        Role.RoleName targetUserRole = salesUser.getRole() != null ? salesUser.getRole().getName() : null;

        // Validate organizations: for SALES targets we enforce exclusive ownership,
        // but for PRESALES targets we allow attaching organizations without changing owner.
        for (Organization org : organizations) {
            if (grandfatheredIds.contains(org.getId())) {
                continue; // Allow legacy assignments to stay linked
            }

            User owner = org.getOwner();

            // If the target is for a PRESALES user, do not change ownership and do not block
            // if the organization is already owned by a SALES user – PRESALES targets may
            // work with any organization's deals.
            if (targetUserRole != Role.RoleName.SALES) {
                continue;
            }

            // SALES target: keep a single SALES owner per organization, but allow
            // reassignment instead of blocking with an error.
            if (owner == null) {
                org.setOwner(salesUser);
                continue;
            }

            if (!owner.getId().equals(salesUser.getId())) {
                Role.RoleName ownerRole = owner.getRole() != null ? owner.getRole().getName() : null;
                // Whether the current owner is another SALES user or a non-sales role,
                // we now reassign the organization to the selected sales user instead
                // of throwing an error. This ensures the "Set Target" flow works even
                // when organizations were previously owned by someone else.
                org.setOwner(salesUser);
            }
        }
        
        return new HashSet<>(organizations);
    }

    @Deprecated
    private LocalDate resolveMonthStart(Integer year, Integer month) {
        try {
            return YearMonth.of(year, month).atDay(1);
        } catch (Exception ex) {
            throw new BadRequestException("Invalid month/year combination");
        }
    }

    private void ensureYearAllowed(int year) {
        if (year < minTargetYear) {
            throw new BadRequestException("Targets can only be created for " + minTargetYear + " onwards");
        }
    }

    private YearMonth resolveListYearMonth(TargetDtos.TargetListFilter filter) {
        if (filter.month != null && filter.year != null) {
            ensureYearAllowed(filter.year);
            return YearMonth.of(filter.year, filter.month);
        }
        YearMonth current = YearMonth.now(zoneId);
        ensureYearAllowed(current.getYear());
        return current;
    }

    private TargetDtos.TargetResponse toResponse(SalesTarget target, Role.RoleName role) {
        TargetDtos.TargetResponse response = new TargetDtos.TargetResponse();
        response.id = target.getId();
        response.userId = target.getUser() != null ? target.getUser().getId() : null;
        response.userName = target.getUser() != null
                ? (target.getUser().getFirstName() + " " + target.getUser().getLastName()).trim()
                : null;
        response.category = target.getCategory() != null ? target.getCategory().getCode() : null;
        
        // Period information
        YearMonth ym = target.getPeriodStart() != null ? YearMonth.from(target.getPeriodStart()) : null;
        if (ym != null) {
            response.year = ym.getYear();
            
            // Extract period-specific fields based on period type
            if (target.getPeriodType() == SalesTarget.PeriodType.MONTHLY) {
                response.month = ym.getMonthValue();
            } else if (target.getPeriodType() == SalesTarget.PeriodType.QUARTERLY) {
                response.month = ym.getMonthValue();
                response.quarter = (ym.getMonthValue() - 1) / 3 + 1;
            } else if (target.getPeriodType() == SalesTarget.PeriodType.HALF_YEARLY) {
                response.month = ym.getMonthValue();
                response.halfYear = ym.getMonthValue() <= 6 ? 1 : 2;
            }
        }
        
        response.targetAmount = target.getTargetAmount();
        response.editable = ym == null || isEditableForRole(ym, role);
        
        // Organizations
        if (target.getOrganizations() != null && !target.getOrganizations().isEmpty()) {
            response.organizationIds = target.getOrganizations().stream()
                    .map(Organization::getId)
                    .collect(Collectors.toList());
            response.organizations = target.getOrganizations().stream()
                    .map(this::toOrganizationSummary)
                    .collect(Collectors.toList());
        } else {
            response.organizationIds = new ArrayList<>();
            response.organizations = new ArrayList<>();
        }
        
        return response;
    }

    private TargetDtos.OrganizationSummary toOrganizationSummary(Organization org) {
        TargetDtos.OrganizationSummary summary = new TargetDtos.OrganizationSummary();
        summary.id = org.getId();
        summary.name = org.getName();
        summary.category = org.getCategory() != null ? org.getCategory().getDbValue() : null;
        return summary;
    }

    private TargetDtos.TargetRow toTargetRow(
            SalesTarget target,
            Map<YearMonth, Map<TargetCategory, Map<Long, DealAggregate>>> achievedMap,
            Map<YearMonth, Map<Long, DealAggregate>> presalesAllCategoryMap,
            YearMonth month,
            TargetCategory category) {
        TargetDtos.TargetRow row = new TargetDtos.TargetRow();
        Long userId = target.getUser() != null ? target.getUser().getId() : null;

        row.userId = userId;
        row.userName = target.getUser() != null
                ? (target.getUser().getFirstName() + " " + target.getUser().getLastName()).trim()
                : null;
        row.userRole = target.getUser() != null && target.getUser().getRole() != null
                ? target.getUser().getRole().getName().name()
                : null;

        DealAggregate aggregate = achievedMap
                .getOrDefault(month, Collections.emptyMap())
                .getOrDefault(category, Collections.emptyMap())
                .get(userId);

        // For PRESALES users we intentionally base Achieved / Total Deals on
        // all won deals under their linked SALES manager (all categories and
        // sources), mirroring the detail API behaviour. We read those values
        // from the category‑agnostic presalesAllCategoryMap.
        DealAggregate presalesAllAggregate = presalesAllCategoryMap != null
                ? presalesAllCategoryMap
                        .getOrDefault(month, Collections.emptyMap())
                        .get(userId)
                : null;

        Role.RoleName roleName = target.getUser() != null && target.getUser().getRole() != null
                ? target.getUser().getRole().getName()
                : null;

        // Default target amount from entity
        BigDecimal rawTargetAmount = target.getTargetAmount() != null
                ? target.getTargetAmount()
                : BigDecimal.ZERO;

        if (roleName == Role.RoleName.PRESALES) {
            // PRESALES: target represents number of diverted deals to be won
            int diversionDeals = presalesAllAggregate != null ? presalesAllAggregate.diversionDeals : 0;
            int totalDeals = presalesAllAggregate != null ? presalesAllAggregate.dealsCount : 0;
            int directDeals = presalesAllAggregate != null ? presalesAllAggregate.directDeals : 0;

            int targetCount = rawTargetAmount.intValue();
            int achievedCount = diversionDeals;

            row.totalTarget = BigDecimal.valueOf(targetCount);
            row.achieved = BigDecimal.valueOf(achievedCount);
            // Total deals should include all won deals (all sources).
            row.totalDeals = totalDeals;

            // Achievement % based on diverted deals vs target (counts)
            BigDecimal achievedBd = BigDecimal.valueOf(achievedCount);
            BigDecimal targetBd = BigDecimal.valueOf(targetCount);
            row.achievementPercent = calculateAchievementPercent(achievedBd, targetBd);

            // Incentive for PRESALES: based on direct/diversion deal counts and target attainment
            BigDecimal baseIncentive = BigDecimal.valueOf(500L)
                    .multiply(BigDecimal.valueOf(directDeals))
                    .add(BigDecimal.valueOf(1000L).multiply(BigDecimal.valueOf(diversionDeals)));

            BigDecimal multiplier = BigDecimal.ZERO;
            if (baseIncentive.compareTo(BigDecimal.ZERO) > 0) {
                if (targetCount > 0) {
                    if (achievedCount > targetCount) {
                        multiplier = BigDecimal.valueOf(1.15); // 115%
                    } else if (achievedCount == targetCount) {
                        multiplier = BigDecimal.ONE; // 100%
                    } else {
                        multiplier = BigDecimal.valueOf(0.75); // 75%
                    }
                } else {
                    // No target configured – treat as neutral (100%)
                    multiplier = BigDecimal.ONE;
                }
            }

            row.incentivePercent = BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
            row.incentiveAmount = baseIncentive.multiply(multiplier).setScale(2, RoundingMode.HALF_UP);
        } else {
            // SALES (or default): original value-based incentive logic
            row.totalTarget = rawTargetAmount;

            BigDecimal achieved = aggregate != null ? aggregate.achieved : BigDecimal.ZERO;
            row.achieved = achieved;
            row.totalDeals = aggregate != null ? aggregate.dealsCount : 0;
            row.achievementPercent = calculateAchievementPercent(achieved, rawTargetAmount);
            row.incentivePercent = calculateIncentive(row.achievementPercent);
            row.incentiveAmount = row.incentivePercent.multiply(achieved)
                    .divide(BigDecimal.valueOf(100), 2, RoundingMode.HALF_UP);
        }

        return row;
    }

    private BigDecimal calculateAchievementPercent(BigDecimal achieved, BigDecimal target) {
        if (target == null || target.compareTo(BigDecimal.ZERO) == 0) {
            return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        }
        return achieved.multiply(BigDecimal.valueOf(100))
                .divide(target, 2, RoundingMode.HALF_UP);
    }

    private BigDecimal calculateIncentive(BigDecimal achievementPercent) {
        if (achievementPercent == null) {
            return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        }
        // New SALES incentive slabs:
        // < 80%         -> 0%
        // 80% - <100%   -> 8%
        // 100% - <150%  -> 10%
        // >= 150%       -> 15%
        if (achievementPercent.compareTo(BigDecimal.valueOf(80)) < 0) {
            return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        }
        if (achievementPercent.compareTo(BigDecimal.valueOf(100)) < 0) {
            return BigDecimal.valueOf(8).setScale(2, RoundingMode.HALF_UP);
        }
        if (achievementPercent.compareTo(BigDecimal.valueOf(150)) < 0) {
            return BigDecimal.TEN.setScale(2, RoundingMode.HALF_UP);
        }
        return BigDecimal.valueOf(15).setScale(2, RoundingMode.HALF_UP);
    }

    /**
     * Resolve the "owner" user for target / incentive calculations.
     *
     * Business rule update:
     * - Attribute won deals to the user who owns the pipeline on which the deal
     *   is won (via the pipeline's team manager) whenever possible.
     * - Fall back to organization owner, then person owner only when a
     *   pipeline / team context is not available.
     */
    private User resolveDealOwner(Deal deal) {
        if (deal == null) {
            return null;
        }

        // 1) Primary: pipeline's team manager (pipeline "owner" for targets)
        if (deal.getPipeline() != null
                && deal.getPipeline().getTeam() != null
                && deal.getPipeline().getTeam().getManager() != null) {
            return deal.getPipeline().getTeam().getManager();
        }

        // 2) Fallback: organization owner
        if (deal.getOrganization() != null && deal.getOrganization().getOwner() != null) {
            return deal.getOrganization().getOwner();
        }

        // 3) Final fallback: person owner
        if (deal.getPerson() != null && deal.getPerson().getOwner() != null) {
            return deal.getPerson().getOwner();
        }

        return null;
    }

    private TargetDtos.DealSummary toDealSummary(Deal deal, User owner, TargetCategory category) {
        TargetDtos.DealSummary summary = new TargetDtos.DealSummary();
        summary.dealId = deal.getId();
        summary.dealName = deal.getName();
        summary.dealValue = safeValue(deal.getValue());
        summary.commissionAmount = safeValue(deal.getCommissionAmount());
        // Prefer the new DealSource enum (set via the WON dialog) so that the
        // Won Deals table on Sales / Pre‑Sales target pages shows the same
        // source that users select when marking a deal as WON. Fall back to the
        // legacy Source entity only when the enum has not been populated.
        if (deal.getDealSource() != null) {
            summary.dealSource = deal.getDealSource().getDisplayName();
        } else if (deal.getSource() != null) {
            summary.dealSource = deal.getSource().getType();
        } else {
            summary.dealSource = null;
        }
        summary.venue = deal.getVenue();
        summary.eventDate = deal.getEventDate() != null ? deal.getEventDate().toString() : null;
        // Won date – mirror the reference timestamp used elsewhere in target
        // calculations (updatedAt when present, otherwise createdAt).
        java.time.LocalDateTime wonReference =
                deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt();
        summary.wonDate = wonReference != null ? wonReference.toLocalDate().toString() : null;
        summary.phoneNumber = resolvePhone(deal);
        summary.category = category != null ? category.getLabel() : null;
        summary.organization = resolveOrganizationName(deal);
        summary.instagramId = deal.getPerson() != null ? deal.getPerson().getInstagramId() : null;
        summary.personSource = resolvePersonSource(deal.getPerson());
        summary.userId = owner != null ? owner.getId() : null;
        summary.userName = owner != null ? (owner.getFirstName() + " " + owner.getLastName()).trim() : null;
        return summary;
    }

    private String resolvePhone(Deal deal) {
        if (deal.getPhoneNumber() != null && !deal.getPhoneNumber().isBlank()) {
            return deal.getPhoneNumber();
        }
        if (deal.getPerson() != null && deal.getPerson().getPhone() != null) {
            return deal.getPerson().getPhone();
        }
        return deal.getContactNumber();
    }

    private String resolveOrganizationName(Deal deal) {
        if (deal.getOrganization() != null) {
            return deal.getOrganization().getName();
        }
        if (deal.getPerson() != null && deal.getPerson().getOrganization() != null) {
            return deal.getPerson().getOrganization().getName();
        }
        return null;
    }

    private String resolvePersonSource(Person person) {
        if (person == null || person.getSource() == null) {
            return null;
        }
        return person.getSource().getDisplayName();
    }

    private TargetDtos.AppliedFilters buildAppliedFilters(TargetDtos.DashboardFilter filter,
                                                          List<YearMonth> months,
                                                          Role.RoleName role) {
        TargetDtos.AppliedFilters applied = new TargetDtos.AppliedFilters();
        applied.timePreset = (filter.preset != null ? filter.preset.name() : TargetTimePreset.THIS_MONTH.name());
        applied.category = filter.category != null ? filter.category.getCode() : null;

        YearMonth first = months.get(0);
        YearMonth last = months.get(months.size() - 1);

        applied.month = filter.month != null ? filter.month : first.getMonthValue();
        applied.year = filter.year != null ? filter.year : first.getYear();
        applied.fromMonth = filter.fromMonth != null ? filter.fromMonth : first.getMonthValue();
        applied.fromYear = filter.fromYear != null ? filter.fromYear : first.getYear();
        applied.toMonth = filter.toMonth != null ? filter.toMonth : last.getMonthValue();
        applied.toYear = filter.toYear != null ? filter.toYear : last.getYear();

        applied.editableForCurrentUser = months.stream().allMatch(month -> isEditableForRole(month, role));
        return applied;
    }

    private boolean isEditableForRole(YearMonth month, Role.RoleName role) {
        if (role == Role.RoleName.ADMIN) {
            return true;
        }
        YearMonth current = YearMonth.now(zoneId);
        return !month.isBefore(current);
    }

    private BigDecimal safeValue(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }

    private Role.RoleName getCurrentUserRole() {
        return requireCurrentUser().getRole().getName();
    }

    private User requireCurrentUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null) {
            throw new UnauthorizedException("User not authenticated");
        }
        String email;
        Object principal = authentication.getPrincipal();
        if (principal instanceof UserDetails userDetails) {
            email = userDetails.getUsername();
        } else if (principal instanceof String str) {
            email = str;
        } else {
            throw new UnauthorizedException("Unable to resolve current user");
        }

        return userRepository.findByEmail(email)
                .orElseThrow(() -> new UnauthorizedException("User not found for email " + email));
    }

    private TargetTimePreset inferPreset(TargetDtos.DashboardFilter filter) {
        if (filter.fromMonth != null && filter.fromYear != null
                && filter.toMonth != null && filter.toYear != null) {
            return TargetTimePreset.CUSTOM_RANGE;
        }
        if (filter.month != null && filter.year != null) {
            return TargetTimePreset.CUSTOM_MONTH;
        }
        return TargetTimePreset.THIS_MONTH;
    }

    private List<YearMonth> resolveMonths(TargetDtos.DashboardFilter filter) {
        TargetTimePreset preset = filter.preset != null ? filter.preset : TargetTimePreset.THIS_MONTH;
        YearMonth reference = (filter.year != null && filter.month != null)
                ? YearMonth.of(filter.year, filter.month)
                : YearMonth.now(zoneId);

        switch (preset) {
            case PREVIOUS_MONTH:
                YearMonth previous = reference.minusMonths(1);
                ensureYearAllowed(previous.getYear());
                return List.of(previous);
            case NEXT_MONTH:
                YearMonth next = reference.plusMonths(1);
                ensureYearAllowed(next.getYear());
                return List.of(next);
            case THIS_QUARTER:
                return quarterMonths(reference);
            case HALF_YEAR:
                return halfYearMonths(reference);
            case THIS_YEAR:
                return yearMonths(reference.getYear());
            case CUSTOM_MONTH:
                ensureYearAllowed(reference.getYear());
                return List.of(reference);
            case CUSTOM_RANGE:
                return customRangeMonths(filter);
            case THIS_MONTH:
            default:
                ensureYearAllowed(reference.getYear());
                return List.of(reference);
        }
    }

    private List<YearMonth> quarterMonths(YearMonth ref) {
        int quarterStartMonth = ((ref.getMonthValue() - 1) / 3) * 3 + 1;
        YearMonth start = YearMonth.of(ref.getYear(), quarterStartMonth);
        ensureYearAllowed(start.getYear());
        return List.of(start, start.plusMonths(1), start.plusMonths(2));
    }

    private List<YearMonth> halfYearMonths(YearMonth ref) {
        int startMonth = ref.getMonthValue() <= 6 ? 1 : 7;
        YearMonth start = YearMonth.of(ref.getYear(), startMonth);
        ensureYearAllowed(start.getYear());
        List<YearMonth> months = new ArrayList<>();
        for (int i = 0; i < 6; i++) {
            months.add(start.plusMonths(i));
        }
        return months;
    }

    private List<YearMonth> yearMonths(int year) {
        ensureYearAllowed(year);
        List<YearMonth> months = new ArrayList<>();
        for (int m = 1; m <= 12; m++) {
            months.add(YearMonth.of(year, m));
        }
        return months;
    }

    private List<YearMonth> customRangeMonths(TargetDtos.DashboardFilter filter) {
        if (filter.fromYear == null || filter.fromMonth == null || filter.toYear == null || filter.toMonth == null) {
            throw new BadRequestException("fromMonth/fromYear and toMonth/toYear are required for range filter");
        }
        YearMonth start = YearMonth.of(filter.fromYear, filter.fromMonth);
        YearMonth end = YearMonth.of(filter.toYear, filter.toMonth);
        if (end.isBefore(start)) {
            throw new BadRequestException("Range end must be after start");
        }
        ensureYearAllowed(start.getYear());
        ensureYearAllowed(end.getYear());

        List<YearMonth> months = new ArrayList<>();
        YearMonth pointer = start;
        while (!pointer.isAfter(end)) {
            months.add(pointer);
            pointer = pointer.plusMonths(1);
        }
        return months;
    }
    
    private LocalDate resolveTargetQueryStart(LocalDate periodStart) {
        LocalDate minAllowedStart = YearMonth.of(minTargetYear, 1).atDay(1);
        LocalDate queryStart = periodStart.minusMonths(MAX_PERIOD_MONTHS - 1L);
        return queryStart.isBefore(minAllowedStart) ? minAllowedStart : queryStart;
    }

    @Override
    @Transactional(readOnly = true)
    public List<TargetDtos.SalesUserWithOrganizations> getSalesUsersWithOrganizations() {
        List<User> salesUsers = userRepository.findByRole_NameAndActiveTrue(Role.RoleName.SALES);
        
        return salesUsers.stream()
                .map(user -> {
                    TargetDtos.SalesUserWithOrganizations response = new TargetDtos.SalesUserWithOrganizations();
                    response.userId = user.getId();
                    response.userName = (user.getFirstName() + " " + user.getLastName()).trim();
                    response.email = user.getEmail();
                    
                    List<Organization> organizations = organizationRepository.findByOwner_Id(user.getId());
                    response.organizations = organizations.stream()
                            .map(this::toOrganizationSummary)
                            .collect(Collectors.toList());
                    
                    return response;
                })
                .collect(Collectors.toList());
    }

    @Override
    @Transactional(readOnly = true)
    public TargetDtos.SalesUserOrganizationsResponse getSalesUserOrganizations(Long userId) {
        User requestedUser = userRepository.findById(userId)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with id " + userId));

        // Resolve the SALES user whose organizations should be used.
        // - If the provided user is SALES → use them directly.
        // - If the provided user is PRESALES → use their manager when the manager has SALES role.
        // This allows the frontend to call this endpoint with either the sales manager id or
        // a pre‑sales user id and still receive the correct organizations for the linked sales team.
        Role.RoleName requestedRole = requestedUser.getRole() != null
                ? requestedUser.getRole().getName()
                : null;

        User salesUser;
        if (requestedRole == Role.RoleName.SALES) {
            salesUser = requestedUser;
        } else if (requestedRole == Role.RoleName.PRESALES
                && requestedUser.getManager() != null
                && requestedUser.getManager().getRole() != null
                && requestedUser.getManager().getRole().getName() == Role.RoleName.SALES) {
            salesUser = requestedUser.getManager();
        } else {
            throw new BadRequestException("User must be SALES or a PRESALES user linked to a SALES manager");
        }

        TargetDtos.SalesUserOrganizationsResponse response = new TargetDtos.SalesUserOrganizationsResponse();
        response.userId = salesUser.getId();
        response.userName = (salesUser.getFirstName() + " " + salesUser.getLastName()).trim();
        response.email = salesUser.getEmail();

        // --- Resolve organizations for the sales team context ---
        //
        // Primary source: organizations explicitly owned by the SALES user.
        // Fallback / augmentation: organizations linked to this user's targets.
        // This ensures that as long as targets are configured with organizations,
        // the frontend will always receive at least one OrganizationSummary with
        // name + category, even if ownership has not been fully normalized.
        List<Organization> ownedOrganizations = organizationRepository.findByOwner_Id(salesUser.getId());

        // Use a LinkedHashMap keyed by id to de‑duplicate while preserving
        // a stable ordering for the frontend.
        Map<Long, Organization> organizationsById = new LinkedHashMap<>();
        for (Organization org : ownedOrganizations) {
            if (org != null && org.getId() != null) {
                organizationsById.put(org.getId(), org);
            }
        }

        // Augment with organizations that are attached to any SalesTarget for this
        // SALES user. This mirrors how the sales user's own detail page infers
        // category + organization context from targets.
        List<SalesTarget> userTargets = salesTargetRepository.findByUser_Id(salesUser.getId());
        for (SalesTarget target : userTargets) {
            if (target.getOrganizations() == null) {
                continue;
            }
            for (Organization org : target.getOrganizations()) {
                if (org != null && org.getId() != null) {
                    organizationsById.putIfAbsent(org.getId(), org);
                }
            }
        }

        List<Organization> organizations = new ArrayList<>(organizationsById.values());
        response.organizations = organizations.stream()
                .map(this::toOrganizationSummary)
                .collect(Collectors.toList());
        
        // Group organizations by month (based on when they were created/assigned).
        // For now, we'll group all organizations together, but this can be enhanced
        // to show month-wise distribution if needed.
        Map<String, List<TargetDtos.OrganizationSummary>> orgsByMonth = new LinkedHashMap<>();
        for (Organization org : organizations) {
            String monthKey = org.getCreatedAt() != null 
                    ? YearMonth.from(org.getCreatedAt().atZone(zoneId).toLocalDate()).toString()
                    : YearMonth.now(zoneId).toString();
            orgsByMonth.computeIfAbsent(monthKey, k -> new ArrayList<>())
                    .add(toOrganizationSummary(org));
        }
        response.organizationsByMonth = orgsByMonth;

        // Derive default category and organizations from the resolved
        // organizations list. This is the canonical "sales team" context that
        // the Pre‑Sales target detail page should mirror.
        if (!organizations.isEmpty()) {
            Set<TargetCategory> orgCategories = organizations.stream()
                    .map(Organization::getCategory)
                    .filter(Objects::nonNull)
                    .map(cat -> TargetCategory.fromValue(cat.getDbValue()))
                    .filter(Objects::nonNull)
                    .collect(Collectors.toCollection(() -> EnumSet.noneOf(TargetCategory.class)));

            if (orgCategories.size() == 1) {
                TargetCategory singleCategory = orgCategories.iterator().next();
                response.defaultCategoryCode = singleCategory.getCode();
                response.defaultCategoryLabel = singleCategory.getLabel();
            }

            response.defaultOrganizationIds = organizations.stream()
                    .map(Organization::getId)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        }
        
        return response;
    }
    
    @Override
    @Transactional(readOnly = true)
    public TargetDtos.TargetUserMonthlyDetailResponse getUserMonthlyDetail(Long userId, int year) {
        ensureYearAllowed(year);

        // Enforce role-based visibility:
        // - ADMIN can see any SALES/PRESALES user's targets.
        // - CATEGORY_MANAGER can see self + SALES/PRESALES under them (same hierarchy as dashboard filters).
        // - Other roles are not allowed to call this endpoint.
        User currentUser = requireCurrentUser();
        Role.RoleName currentRole = currentUser.getRole() != null
                ? currentUser.getRole().getName()
                : null;

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with id " + userId));
        Role.RoleName userRole = user.getRole() != null ? user.getRole().getName() : null;
        if (userRole != Role.RoleName.SALES && userRole != Role.RoleName.PRESALES) {
            throw new BadRequestException("User must have SALES or PRESALES role");
        }

        // For PRESALES users, we attribute deals via their linked SALES manager.
        // This ensures that the Pre‑Sales detail page can show a realistic Won
        // Deals table (Direct, Divert, Instagram, Reference, Planner, etc.)
        // even though the underlying deals are still owned by the SALES user.
        User presalesManager = null;
        if (userRole == Role.RoleName.PRESALES
                && user.getManager() != null
                && user.getManager().getRole() != null
                && user.getManager().getRole().getName() == Role.RoleName.SALES) {
            presalesManager = user.getManager();
        }

        // Admin can always view, others must follow hierarchy rules.
        if (currentRole != Role.RoleName.ADMIN) {
            // Reuse the same access rules used for the dashboard user filter.
            boolean allowed = false;
            if (Objects.equals(currentUser.getId(), user.getId())) {
                allowed = true;
            } else if (currentRole == Role.RoleName.CATEGORY_MANAGER || currentRole == Role.RoleName.SALES) {
                allowed = isAccessibleForTargets(currentUser, user, currentRole);
            }

            if (!allowed) {
                throw new ForbiddenException("You are not allowed to view targets for this user");
            }
        }
        
        List<YearMonth> months = yearMonths(year);
        LocalDate startDate = YearMonth.of(year, 1).atDay(1);
        LocalDate endDate = YearMonth.of(year, 12).atEndOfMonth();
        
        List<SalesTarget> targets = salesTargetRepository.findByUser_IdAndPeriodStartBetween(
                userId, startDate, endDate);
        
        // Collect unique categories and organizations from targets
        Set<TargetCategory> uniqueCategories = new HashSet<>();
        Set<Organization> uniqueOrganizations = new HashSet<>();
        Map<YearMonth, BigDecimal> monthlyTargets = new HashMap<>();
        for (SalesTarget target : targets) {
            // Collect category
            if (target.getCategory() != null) {
                uniqueCategories.add(target.getCategory());
            }
            // Collect organizations
            if (target.getOrganizations() != null && !target.getOrganizations().isEmpty()) {
                uniqueOrganizations.addAll(target.getOrganizations());
            }
            
            List<YearMonth> coverage = expandTargetMonths(target);
            if (coverage.isEmpty()) {
                continue;
            }
            BigDecimal amount = safeValue(target.getTargetAmount());
            BigDecimal monthlyShare = amount.divide(BigDecimal.valueOf(coverage.size()), 2, RoundingMode.HALF_UP);
            for (YearMonth covered : coverage) {
                if (covered.getYear() != year) {
                    continue;
                }
                monthlyTargets.merge(covered, monthlyShare, BigDecimal::add);
            }
        }
        
        LocalDateTime dealsStart = startDate.atStartOfDay();
        LocalDateTime dealsEnd = endDate.plusDays(1).atStartOfDay();
        List<Deal> deals = dealRepository.findWonDealsUpdatedBetween(dealsStart, dealsEnd);

        // Per-user won deals list for the detail page (Won Deals table)
        List<TargetDtos.DealSummary> userDeals = new ArrayList<>();

        Map<YearMonth, MonthlyDealAggregate> monthlyDeals = new HashMap<>();
        for (Deal deal : deals) {
            User owner = resolveDealOwner(deal);
            if (owner == null || owner.getId() == null) {
                continue;
            }

            boolean include;
            if (userRole == Role.RoleName.SALES) {
                include = owner.getId().equals(userId);
            } else {
                // PRESALES: include deals under their linked SALES manager
                include = presalesManager != null && owner.getId().equals(presalesManager.getId());
            }
            if (!include) {
                continue;
            }

            // Build per-deal summary attributed to the current user:
            // - SALES: attribute to the sales owner (existing behaviour)
            // - PRESALES: attribute to the presales user so frontend receives
            //   deals keyed by the presales userId.
            TargetCategory dealCategory = TargetCategory.fromDeal(deal);
            if (dealCategory != null) {
                TargetDtos.DealSummary summary = (userRole == Role.RoleName.SALES)
                        ? toDealSummary(deal, owner, dealCategory)
                        : toDealSummary(deal, user, dealCategory);
                userDeals.add(summary);
            }
            LocalDateTime reference = deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt();
            if (reference == null) {
                continue;
            }
            YearMonth ym = YearMonth.from(reference);
            if (ym.getYear() != year) {
                continue;
            }
            
            MonthlyDealAggregate aggregate = monthlyDeals.computeIfAbsent(ym, key -> new MonthlyDealAggregate());
            BigDecimal value = safeValue(deal.getValue());
            aggregate.achieved = aggregate.achieved.add(value);
            aggregate.totalDeals += 1;

            // A deal is considered "diverted" if either isDiverted flag is true OR dealSource is DIVERT
            // Use the canonical diverted flag on the Deal entity for diversion
            // counts, and fall back to source-based classification only for
            // secondary breakdowns (Instagram / Reference / Planner).
            boolean diverted = Boolean.TRUE.equals(deal.getIsDiverted()) 
                    || (deal.getDealSource() != null && deal.getDealSource() == DealSource.DIVERT);
            if (diverted) {
                aggregate.diversionDeals += 1;
            } else {
                String source = normalizeSource(resolveDealSourceLabel(deal));
                if ("INSTA".equals(source) || "INSTAGRAM".equals(source)) {
                    aggregate.instaDeals += 1;
                } else if ("REFERENCE".equals(source) || "REFERRAL".equals(source)) {
                    aggregate.referenceDeals += 1;
                } else if ("PLANNER".equals(source)) {
                    aggregate.plannerDeals += 1;
                }
            }
        }
        
        List<TargetDtos.UserMonthlyBreakdown> rows = new ArrayList<>();
        for (YearMonth ym : months) {
            BigDecimal targetValue = monthlyTargets.get(ym);
            MonthlyDealAggregate aggregate = monthlyDeals.get(ym);

            TargetDtos.UserMonthlyBreakdown breakdown = new TargetDtos.UserMonthlyBreakdown();
            breakdown.month = ym.getMonthValue();
            breakdown.year = ym.getYear();

            if (userRole == Role.RoleName.PRESALES) {
                int diversionDeals = aggregate != null ? aggregate.diversionDeals : 0;
                int totalDeals = aggregate != null ? aggregate.totalDeals : 0;
                int directDeals = totalDeals - diversionDeals;

                int targetCount = targetValue != null ? targetValue.intValue() : 0;
                int achievedCount = diversionDeals;

                BigDecimal targetBd = targetCount > 0
                        ? BigDecimal.valueOf(targetCount).setScale(2, RoundingMode.HALF_UP)
                        : null;
                BigDecimal achievedBd = BigDecimal.valueOf(achievedCount).setScale(2, RoundingMode.HALF_UP);

                breakdown.target = targetBd;
                breakdown.achieved = achievedBd;

                if (targetCount > 0) {
                    breakdown.achievementPercent = calculateAchievementPercent(achievedBd, BigDecimal.valueOf(targetCount));
                } else {
                    breakdown.achievementPercent = BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
                }

                // Incentive for PRESALES
                BigDecimal baseIncentive = BigDecimal.valueOf(500L)
                        .multiply(BigDecimal.valueOf(directDeals))
                        .add(BigDecimal.valueOf(1000L).multiply(BigDecimal.valueOf(diversionDeals)));

                BigDecimal multiplier = BigDecimal.ZERO;
                if (baseIncentive.compareTo(BigDecimal.ZERO) > 0) {
                    if (targetCount > 0) {
                        if (achievedCount > targetCount) {
                            multiplier = BigDecimal.valueOf(1.15); // 115%
                        } else if (achievedCount == targetCount) {
                            multiplier = BigDecimal.ONE; // 100%
                        } else {
                            multiplier = BigDecimal.valueOf(0.75); // 75%
                        }
                    } else {
                        // No target configured – treat as neutral (100%)
                        multiplier = BigDecimal.ONE;
                    }
                }
                breakdown.incentive = baseIncentive.multiply(multiplier).setScale(2, RoundingMode.HALF_UP);

                breakdown.totalDeals = totalDeals;
                breakdown.diversionDeals = diversionDeals;
                breakdown.instaDeals = aggregate != null ? aggregate.instaDeals : 0;
                breakdown.referenceDeals = aggregate != null ? aggregate.referenceDeals : 0;
                breakdown.plannerDeals = aggregate != null ? aggregate.plannerDeals : 0;
            } else {
                // SALES (or default) behaviour – value-based incentive
                BigDecimal achieved = aggregate != null ? aggregate.achieved : BigDecimal.ZERO;
                BigDecimal achievementPercent = (targetValue == null || targetValue.compareTo(BigDecimal.ZERO) == 0)
                        ? BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP)
                        : calculateAchievementPercent(achieved, targetValue);
                BigDecimal incentivePercent = calculateIncentive(achievementPercent);
                BigDecimal incentiveAmount = achieved.compareTo(BigDecimal.ZERO) == 0
                        ? BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP)
                        : incentivePercent.multiply(achieved)
                        .divide(BigDecimal.valueOf(100), 2, RoundingMode.HALF_UP);

                breakdown.target = targetValue != null ? targetValue.setScale(2, RoundingMode.HALF_UP) : null;
                breakdown.achieved = achieved.setScale(2, RoundingMode.HALF_UP);
                breakdown.achievementPercent = achievementPercent;
                breakdown.totalDeals = aggregate != null ? aggregate.totalDeals : 0;
                breakdown.incentive = incentiveAmount;
                breakdown.diversionDeals = aggregate != null ? aggregate.diversionDeals : 0;
                breakdown.instaDeals = aggregate != null ? aggregate.instaDeals : 0;
                breakdown.referenceDeals = aggregate != null ? aggregate.referenceDeals : 0;
                breakdown.plannerDeals = aggregate != null ? aggregate.plannerDeals : 0;
            }
            rows.add(breakdown);
        }
        
        TargetDtos.TargetUserMonthlyDetailResponse response = new TargetDtos.TargetUserMonthlyDetailResponse();
        response.userId = user.getId();
        response.userName = (user.getFirstName() + " " + user.getLastName()).trim();
        response.year = year;
        
        // Set categories from targets
        List<String> categoryCodes = uniqueCategories.stream()
                .map(TargetCategory::getCode)
                .sorted()
                .collect(Collectors.toList());
        response.categories = categoryCodes;
        response.availableCategories = categoryCodes;
        
        // Set organizations from targets
        List<Organization> orgList = new ArrayList<>(uniqueOrganizations);
        orgList.sort(Comparator.comparing(Organization::getName));
        List<Long> organizationIds = orgList.stream()
                .map(Organization::getId)
                .collect(Collectors.toList());
        List<TargetDtos.OrganizationSummary> organizationSummaries = orgList.stream()
                .map(this::toOrganizationSummary)
                .collect(Collectors.toList());
        response.organizationIds = organizationIds;
        response.availableOrganizationIds = organizationIds;
        response.organizations = organizationSummaries;
        response.availableOrganizations = organizationSummaries;

        // --- Default category & organizations for Goal Details (Sales/Pre‑Sales) ---
        // For PRESALES users, derive defaults primarily from the linked SALES
        // manager's organizations (the "sales team" context). If that is not
        // available, fall back to the categories/organizations directly linked
        // to this user's targets.
        TargetCategory defaultCategory = null;
        List<Long> defaultOrgIds = new ArrayList<>();

        if (userRole == Role.RoleName.PRESALES
                && user.getManager() != null
                && user.getManager().getRole() != null
                && user.getManager().getRole().getName() == Role.RoleName.SALES) {
            User salesManager = user.getManager();
            List<Organization> salesTeamOrgs = organizationRepository.findByOwner_Id(salesManager.getId());

            if (!salesTeamOrgs.isEmpty()) {
                Set<TargetCategory> managerCategories = salesTeamOrgs.stream()
                        .map(Organization::getCategory)
                        .filter(Objects::nonNull)
                        .map(cat -> TargetCategory.fromValue(cat.getDbValue()))
                        .filter(Objects::nonNull)
                        .collect(Collectors.toCollection(() -> EnumSet.noneOf(TargetCategory.class)));

                if (managerCategories.size() == 1) {
                    defaultCategory = managerCategories.iterator().next();
                }

                defaultOrgIds = salesTeamOrgs.stream()
                        .map(Organization::getId)
                        .filter(Objects::nonNull)
                        .collect(Collectors.toList());
            }
        }

        // Fallbacks when we couldn't derive from the sales manager context.
        if (defaultCategory == null && !uniqueCategories.isEmpty()) {
            if (uniqueCategories.size() == 1) {
                defaultCategory = uniqueCategories.iterator().next();
            }
        }
        if (defaultOrgIds.isEmpty() && !uniqueOrganizations.isEmpty()) {
            if (uniqueOrganizations.size() == 1) {
                defaultOrgIds = List.of(uniqueOrganizations.iterator().next().getId());
            }
        }

        if (defaultCategory != null) {
            response.defaultCategoryCode = defaultCategory.getCode();
            response.defaultCategoryLabel = defaultCategory.getLabel();
        }
        response.defaultOrganizationIds = defaultOrgIds;
        response.deals = userDeals;
        response.monthlyData = rows;
        return response;
    }

    private List<YearMonth> expandTargetMonths(SalesTarget target) {
        List<YearMonth> months = new ArrayList<>();
        if (target.getPeriodStart() == null || target.getPeriodType() == null) {
            return months;
        }
        YearMonth start = YearMonth.from(target.getPeriodStart());
        switch (target.getPeriodType()) {
            case MONTHLY -> months.add(start);
            case QUARTERLY -> {
                for (int i = 0; i < 3; i++) {
                    months.add(start.plusMonths(i));
                }
            }
            case HALF_YEARLY -> {
                for (int i = 0; i < 6; i++) {
                    months.add(start.plusMonths(i));
                }
            }
            case YEARLY -> {
                for (int i = 0; i < 12; i++) {
                    months.add(start.plusMonths(i));
                }
            }
        }
        return months;
    }
    
    private String resolveDealSourceLabel(Deal deal) {
        // Prefer the new DealSource enum captured on the deal itself.
        if (deal.getDealSource() != null) {
            return deal.getDealSource().getDisplayName();
        }
        // Fallback to legacy Source entity when present.
        if (deal.getSource() != null && deal.getSource().getType() != null) {
            return deal.getSource().getType();
        }
        if (deal.getPerson() != null && deal.getPerson().getSource() != null) {
            return deal.getPerson().getSource().name();
        }
        return null;
    }
    
    private String normalizeSource(String value) {
        return value == null ? null : value.trim().toUpperCase(Locale.ROOT);
    }
    
    private static class DashboardComputation {
        private List<YearMonth> months;
        private Map<YearMonth, Map<TargetCategory, List<SalesTarget>>> targetsByMonth;
        private Map<YearMonth, Map<TargetCategory, Map<Long, DealAggregate>>> achievedMap;
        private Map<YearMonth, Map<Long, DealAggregate>> presalesAllCategoryMap;
        private List<TargetDtos.DealSummary> dealSummaries;
        private TargetCategory categoryFilter;
    }
    
    private static class DealAggregate {
        private BigDecimal achieved = BigDecimal.ZERO;
        private int dealsCount = 0;
        // Number of diversion and direct deals for PRESALES incentive logic
        private int diversionDeals = 0;
        private int directDeals = 0;
    }
    
    private static class MonthlyDealAggregate {
        private BigDecimal achieved = BigDecimal.ZERO;
        private int totalDeals = 0;
        private int diversionDeals = 0;
        private int instaDeals = 0;
        private int referenceDeals = 0;
        private int plannerDeals = 0;
    }
}

