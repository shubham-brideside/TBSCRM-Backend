package com.brideside.crm.service.impl;

import com.brideside.crm.dto.TargetDtos;
import com.brideside.crm.dto.TargetDtos.TargetTimePreset;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.Person;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.SalesTarget;
import com.brideside.crm.entity.TargetCategory;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.exception.UnauthorizedException;
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

        // Validate user is SALES role
        if (user.getRole() == null || user.getRole().getName() != Role.RoleName.SALES) {
            throw new BadRequestException("Targets can only be set for SALES users");
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
            // Validate user is SALES role
            if (user.getRole() == null || user.getRole().getName() != Role.RoleName.SALES) {
                throw new BadRequestException("Targets can only be set for SALES users");
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
                    rows.add(toTargetRow(categoryTarget, computation.achievedMap, ym, category));
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
                    .map(target -> toTargetRow(target, computation.achievedMap, ym, safeFilter.category))
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

        TargetCategory categoryFilter = filter.category;
        LocalDate startDate = months.get(0).atDay(1);
        LocalDate endDate = months.get(months.size() - 1).atEndOfMonth();
        LocalDate queryStart = resolveTargetQueryStart(startDate);

        List<SalesTarget> targets = categoryFilter != null
                ? salesTargetRepository.findByCategoryAndPeriodStartBetween(categoryFilter, queryStart, endDate)
                : salesTargetRepository.findByPeriodStartBetween(queryStart, endDate);

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
        List<Deal> wonDeals = dealRepository.findWonDealsUpdatedBetween(dealsStart, dealsEnd);
        wonDeals.sort(Comparator.comparing(
                (Deal d) -> d.getUpdatedAt() != null ? d.getUpdatedAt() : d.getCreatedAt(),
                Comparator.nullsLast(Comparator.naturalOrder())
        ).reversed());

        Map<YearMonth, Map<TargetCategory, Map<Long, DealAggregate>>> achievedMap = new HashMap<>();
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

            TargetCategory dealCategory = TargetCategory.fromDeal(deal);
            if (dealCategory == null) {
                continue;
            }
            if (categoryFilter != null && categoryFilter != dealCategory) {
                continue;
            }

            User owner = resolveDealOwner(deal);
            if (owner != null) {
                DealAggregate aggregate = achievedMap
                        .computeIfAbsent(dealMonth, key -> new EnumMap<>(TargetCategory.class))
                        .computeIfAbsent(dealCategory, key -> new HashMap<>())
                        .computeIfAbsent(owner.getId(), key -> new DealAggregate());
                aggregate.achieved = aggregate.achieved.add(safeValue(deal.getValue()));
                aggregate.dealsCount += 1;
            }

            dealSummaries.add(toDealSummary(deal, owner, dealCategory));
        }

        DashboardComputation computation = new DashboardComputation();
        computation.months = months;
        computation.targetsByMonth = targetsByMonth;
        computation.achievedMap = achievedMap;
        computation.dealSummaries = dealSummaries;
        computation.categoryFilter = categoryFilter;
        return computation;
    }

    @Override
    @Transactional(readOnly = true)
    public TargetDtos.FiltersResponse filters() {
        TargetDtos.FiltersResponse response = new TargetDtos.FiltersResponse();
        response.categories = Arrays.stream(TargetCategory.values())
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
        return response;
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
        
        // Validate organizations: allow reassignment from non-sales owners, but block other sales reps
        for (Organization org : organizations) {
            if (grandfatheredIds.contains(org.getId())) {
                continue; // Allow legacy assignments to stay linked
            }
            
            User owner = org.getOwner();
            if (owner == null) {
                org.setOwner(salesUser);
                continue;
            }

            if (!owner.getId().equals(salesUser.getId())) {
                Role.RoleName ownerRole = owner.getRole() != null ? owner.getRole().getName() : null;
                if (ownerRole == Role.RoleName.SALES) {
                    throw new BadRequestException("Organization " + org.getId() + " (" + org.getName() +
                            ") is already assigned to another sales user");
                }
                // Owner is not a sales rep (likely manager/admin) – reassign to the selected sales user
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
            YearMonth month,
            TargetCategory category) {

        TargetDtos.TargetRow row = new TargetDtos.TargetRow();
        Long userId = target.getUser() != null ? target.getUser().getId() : null;

        row.userId = userId;
        row.userName = target.getUser() != null
                ? (target.getUser().getFirstName() + " " + target.getUser().getLastName()).trim()
                : null;
        row.totalTarget = target.getTargetAmount();

        DealAggregate aggregate = achievedMap
                .getOrDefault(month, Collections.emptyMap())
                .getOrDefault(category, Collections.emptyMap())
                .get(userId);

        BigDecimal achieved = aggregate != null ? aggregate.achieved : BigDecimal.ZERO;
        row.achieved = achieved;
        row.totalDeals = aggregate != null ? aggregate.dealsCount : 0;
        row.achievementPercent = calculateAchievementPercent(achieved, target.getTargetAmount());
        row.incentivePercent = calculateIncentive(row.achievementPercent);
        row.incentiveAmount = row.incentivePercent.multiply(achieved)
                .divide(BigDecimal.valueOf(100), 2, RoundingMode.HALF_UP);

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
        if (achievementPercent.compareTo(BigDecimal.valueOf(80)) < 0) {
            return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        }
        if (achievementPercent.compareTo(BigDecimal.valueOf(100)) <= 0) {
            return BigDecimal.TEN.setScale(2, RoundingMode.HALF_UP);
        }
        return BigDecimal.valueOf(15).setScale(2, RoundingMode.HALF_UP);
    }

    private User resolveDealOwner(Deal deal) {
        if (deal == null) {
            return null;
        }
        if (deal.getPerson() != null && deal.getPerson().getOwner() != null) {
            return deal.getPerson().getOwner();
        }
        if (deal.getOrganization() != null && deal.getOrganization().getOwner() != null) {
            return deal.getOrganization().getOwner();
        }
        return null;
    }

    private TargetDtos.DealSummary toDealSummary(Deal deal, User owner, TargetCategory category) {
        TargetDtos.DealSummary summary = new TargetDtos.DealSummary();
        summary.dealId = deal.getId();
        summary.dealName = deal.getName();
        summary.dealValue = safeValue(deal.getValue());
        summary.commissionAmount = safeValue(deal.getCommissionAmount());
        summary.dealSource = deal.getSource() != null ? deal.getSource().getType() : null;
        summary.venue = deal.getVenue();
        summary.eventDate = deal.getEventDate() != null ? deal.getEventDate().toString() : null;
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
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with id " + userId));
        
        if (user.getRole() == null || user.getRole().getName() != Role.RoleName.SALES) {
            throw new BadRequestException("User must have SALES role");
        }
        
        TargetDtos.SalesUserOrganizationsResponse response = new TargetDtos.SalesUserOrganizationsResponse();
        response.userId = user.getId();
        response.userName = (user.getFirstName() + " " + user.getLastName()).trim();
        response.email = user.getEmail();
        
        List<Organization> organizations = organizationRepository.findByOwner_Id(userId);
        response.organizations = organizations.stream()
                .map(this::toOrganizationSummary)
                .collect(Collectors.toList());
        
        // Group organizations by month (based on when they were created/assigned)
        // For now, we'll group all organizations together, but this can be enhanced
        // to show month-wise distribution if needed
        Map<String, List<TargetDtos.OrganizationSummary>> orgsByMonth = new LinkedHashMap<>();
        for (Organization org : organizations) {
            String monthKey = org.getCreatedAt() != null 
                    ? YearMonth.from(org.getCreatedAt().atZone(zoneId).toLocalDate()).toString()
                    : YearMonth.now(zoneId).toString();
            orgsByMonth.computeIfAbsent(monthKey, k -> new ArrayList<>())
                    .add(toOrganizationSummary(org));
        }
        response.organizationsByMonth = orgsByMonth;
        
        return response;
    }
    
    @Override
    @Transactional(readOnly = true)
    public TargetDtos.TargetUserMonthlyDetailResponse getUserMonthlyDetail(Long userId, int year) {
        ensureYearAllowed(year);
        
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with id " + userId));
        if (user.getRole() == null || user.getRole().getName() != Role.RoleName.SALES) {
            throw new BadRequestException("User must have SALES role");
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
        
        Map<YearMonth, MonthlyDealAggregate> monthlyDeals = new HashMap<>();
        for (Deal deal : deals) {
            User owner = resolveDealOwner(deal);
            if (owner == null || owner.getId() == null || !owner.getId().equals(userId)) {
                continue;
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
            
            String source = normalizeSource(resolveDealSourceLabel(deal));
            if ("DIVERSION".equals(source)) {
                aggregate.diversionDeals += 1;
            } else if ("INSTA".equals(source) || "INSTAGRAM".equals(source)) {
                aggregate.instaDeals += 1;
            } else if ("REFERENCE".equals(source) || "REFERRAL".equals(source)) {
                aggregate.referenceDeals += 1;
            } else if ("PLANNER".equals(source)) {
                aggregate.plannerDeals += 1;
            }
        }
        
        List<TargetDtos.UserMonthlyBreakdown> rows = new ArrayList<>();
        for (YearMonth ym : months) {
            BigDecimal targetValue = monthlyTargets.get(ym);
            MonthlyDealAggregate aggregate = monthlyDeals.get(ym);
            BigDecimal achieved = aggregate != null ? aggregate.achieved : BigDecimal.ZERO;
            BigDecimal achievementPercent = (targetValue == null || targetValue.compareTo(BigDecimal.ZERO) == 0)
                    ? BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP)
                    : calculateAchievementPercent(achieved, targetValue);
            BigDecimal incentivePercent = calculateIncentive(achievementPercent);
            BigDecimal incentiveAmount = achieved.compareTo(BigDecimal.ZERO) == 0
                    ? BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP)
                    : incentivePercent.multiply(achieved)
                    .divide(BigDecimal.valueOf(100), 2, RoundingMode.HALF_UP);
            
            TargetDtos.UserMonthlyBreakdown breakdown = new TargetDtos.UserMonthlyBreakdown();
            breakdown.month = ym.getMonthValue();
            breakdown.year = ym.getYear();
            breakdown.target = targetValue != null ? targetValue.setScale(2, RoundingMode.HALF_UP) : null;
            breakdown.achieved = achieved.setScale(2, RoundingMode.HALF_UP);
            breakdown.achievementPercent = achievementPercent;
            breakdown.totalDeals = aggregate != null ? aggregate.totalDeals : 0;
            breakdown.incentive = incentiveAmount;
            breakdown.diversionDeals = aggregate != null ? aggregate.diversionDeals : 0;
            breakdown.instaDeals = aggregate != null ? aggregate.instaDeals : 0;
            breakdown.referenceDeals = aggregate != null ? aggregate.referenceDeals : 0;
            breakdown.plannerDeals = aggregate != null ? aggregate.plannerDeals : 0;
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
        private List<TargetDtos.DealSummary> dealSummaries;
        private TargetCategory categoryFilter;
    }
    
    private static class DealAggregate {
        private BigDecimal achieved = BigDecimal.ZERO;
        private int dealsCount = 0;
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

