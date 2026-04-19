package com.brideside.crm.service.impl;

import com.brideside.crm.dto.AdminDashboardDtos;
import com.brideside.crm.dto.CategoryManagerDashboardDtos;
import com.brideside.crm.entity.Activity;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.DealStatus;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.User;
import com.brideside.crm.repository.ActivityRepository;
import com.brideside.crm.repository.DealRepository;
import com.brideside.crm.repository.PipelineRepository;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.CategoryManagerDashboardService;
import com.brideside.crm.service.DealAccessScope;
import com.brideside.crm.service.PipelineAccessService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class CategoryManagerDashboardServiceImpl implements CategoryManagerDashboardService {

    private final DealRepository dealRepository;
    private final UserRepository userRepository;
    private final ActivityRepository activityRepository;
    private final PipelineRepository pipelineRepository;
    private final PipelineAccessService pipelineAccessService;

    public CategoryManagerDashboardServiceImpl(DealRepository dealRepository,
                                               UserRepository userRepository,
                                               ActivityRepository activityRepository,
                                               PipelineRepository pipelineRepository,
                                               PipelineAccessService pipelineAccessService) {
        this.dealRepository = dealRepository;
        this.userRepository = userRepository;
        this.activityRepository = activityRepository;
        this.pipelineRepository = pipelineRepository;
        this.pipelineAccessService = pipelineAccessService;
    }

    @Override
    @Transactional(readOnly = true)
    public CategoryManagerDashboardDtos.SummaryResponse getDashboardSummary(User categoryManager) {
        DealAccessScope dealScope = pipelineAccessService.resolveDealAccessScopeForUser(categoryManager);

        long salesManagersCount = 0L;
        long presalesCount = 0L;
        List<User> directReports = userRepository.findByManagerId(categoryManager.getId());
        for (User report : directReports) {
            if (report.getRole() != null) {
                if (report.getRole().getName() == Role.RoleName.SALES) {
                    salesManagersCount++;
                } else if (report.getRole().getName() == Role.RoleName.PRESALES) {
                    presalesCount++;
                }
            }
        }
        for (User sales : directReports) {
            if (sales.getRole() != null && sales.getRole().getName() == Role.RoleName.SALES) {
                List<User> presalesUnder = userRepository.findByManagerId(sales.getId());
                for (User p : presalesUnder) {
                    if (p.getRole() != null && p.getRole().getName() == Role.RoleName.PRESALES) {
                        presalesCount++;
                    }
                }
            }
        }
        long totalTeamSize = salesManagersCount + presalesCount;

        long totalWonDeals = 0L;
        long totalLostDeals = 0L;
        long totalInProgressDeals = 0L;
        BigDecimal totalWonValue = BigDecimal.ZERO;
        BigDecimal totalWonValueYtd = BigDecimal.ZERO;
        int currentYear = LocalDate.now().getYear();

        List<Deal> wonDeals = dealRepository.findByStatusAndIsDeletedFalse(DealStatus.WON);
        for (Deal deal : wonDeals) {
            if (!pipelineAccessService.canAccessDeal(deal, dealScope)) {
                continue;
            }
            totalWonDeals++;
            BigDecimal v = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;
            totalWonValue = totalWonValue.add(v);
            LocalDateTime ref = deal.getWonAt() != null ? deal.getWonAt()
                    : (deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt());
            if (ref != null && ref.getYear() == currentYear) {
                totalWonValueYtd = totalWonValueYtd.add(v);
            }
        }

        List<Deal> lostDeals = dealRepository.findByStatusAndIsDeletedFalse(DealStatus.LOST);
        for (Deal deal : lostDeals) {
            if (!pipelineAccessService.canAccessDeal(deal, dealScope)) {
                continue;
            }
            totalLostDeals++;
        }

        List<Deal> allDeals = dealRepository.findByIsDeletedFalse();
        for (Deal deal : allDeals) {
            if (deal.getStatus() != DealStatus.IN_PROGRESS) {
                continue;
            }
            if (!pipelineAccessService.canAccessDeal(deal, dealScope)) {
                continue;
            }
            totalInProgressDeals++;
        }

        CategoryManagerDashboardDtos.SummaryResponse r = new CategoryManagerDashboardDtos.SummaryResponse();
        r.salesManagersCount = salesManagersCount;
        r.presalesCount = presalesCount;
        r.totalTeamSize = totalTeamSize;
        r.totalWonDeals = totalWonDeals;
        r.totalLostDeals = totalLostDeals;
        r.totalInProgressDeals = totalInProgressDeals;
        r.totalWonValue = totalWonValue;
        r.totalWonValueYtd = totalWonValueYtd;
        return r;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.RevenueSummaryResponse getRevenueSummary(User categoryManager,
            LocalDate dateFrom, LocalDate dateTo) {
        if (dateFrom == null || dateTo == null) {
            throw new IllegalArgumentException("dateFrom and dateTo are required");
        }
        if (dateTo.isBefore(dateFrom)) {
            throw new IllegalArgumentException("dateTo must be on or after dateFrom");
        }
        DealAccessScope dealScope = pipelineAccessService.resolveDealAccessScopeForUser(categoryManager);
        LocalDateTime fromDateTime = dateFrom.atStartOfDay();
        LocalDateTime toDateTime = dateTo.plusDays(1).atStartOfDay();

        List<Deal> wonDeals = dealRepository.findByStatusAndIsDeletedFalse(DealStatus.WON);
        long totalDeals = 0L;
        BigDecimal totalValue = BigDecimal.ZERO;
        Map<String, CategoryRevenueAggregate> byCategory = new HashMap<>();
        Map<Long, UserRevenueAggregate> byUser = new HashMap<>();
        Map<Long, PipelineRevenueAggregate> byPipeline = new HashMap<>();

        for (Deal deal : wonDeals) {
            if (!pipelineAccessService.canAccessDeal(deal, dealScope)) {
                continue;
            }
            User owner = resolveOwnerFromDeal(deal);
            LocalDateTime reference = deal.getWonAt() != null ? deal.getWonAt()
                    : (deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt());
            if (reference == null || reference.isBefore(fromDateTime) || !reference.isBefore(toDateTime)) {
                continue;
            }
            BigDecimal value = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;
            totalDeals++;
            totalValue = totalValue.add(value);

            String categoryKey = null;
            if (deal.getOrganization() != null && deal.getOrganization().getCategory() != null) {
                categoryKey = deal.getOrganization().getCategory().getDbValue();
            }
            if (categoryKey != null) {
                byCategory.computeIfAbsent(categoryKey, k -> new CategoryRevenueAggregate())
                        .add(value);
            }
            if (owner != null && owner.getId() != null) {
                Long userId = owner.getId();
                byUser.computeIfAbsent(userId, id -> new UserRevenueAggregate(owner)).add(value);
            }
            if (deal.getPipeline() != null && deal.getPipeline().getId() != null) {
                Long pipelineId = deal.getPipeline().getId();
                byPipeline.computeIfAbsent(pipelineId, id -> new PipelineRevenueAggregate(deal)).add(value);
            }
        }

        AdminDashboardDtos.RevenueSummaryResponse response = new AdminDashboardDtos.RevenueSummaryResponse();
        response.dateFrom = dateFrom;
        response.dateTo = dateTo;
        response.totalDeals = totalDeals;
        response.totalDealValue = totalValue;
        response.categories = byCategory.entrySet().stream()
                .map(e -> {
                    AdminDashboardDtos.RevenueByCategoryRow row = new AdminDashboardDtos.RevenueByCategoryRow();
                    row.category = e.getKey();
                    row.totalDeals = e.getValue().totalDeals;
                    row.totalDealValue = e.getValue().totalValue;
                    return row;
                })
                .collect(Collectors.toList());
        response.users = new ArrayList<>(byUser.values()).stream()
                .map(agg -> {
                    AdminDashboardDtos.RevenueByUserRow row = new AdminDashboardDtos.RevenueByUserRow();
                    row.userId = agg.userId;
                    row.userName = agg.userName;
                    row.email = agg.email;
                    row.totalDeals = agg.totalDeals;
                    row.totalDealValue = agg.totalValue;
                    return row;
                })
                .collect(Collectors.toList());
        response.pipelines = new ArrayList<>(byPipeline.values()).stream()
                .map(agg -> {
                    AdminDashboardDtos.RevenueByPipelineRow row = new AdminDashboardDtos.RevenueByPipelineRow();
                    row.pipelineId = agg.pipelineId;
                    row.pipelineName = agg.pipelineName;
                    row.totalDeals = agg.totalDeals;
                    row.totalDealValue = agg.totalValue;
                    return row;
                })
                .collect(Collectors.toList());
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.WonDealsBySalesUserResponse getWonDealsBySalesUser(User categoryManager) {
        DealAccessScope dealScope = pipelineAccessService.resolveDealAccessScopeForUser(categoryManager);
        List<Deal> wonDeals = dealRepository.findByStatusAndIsDeletedFalse(DealStatus.WON);
        Map<Long, AdminDashboardDtos.SalesUserWonDealsRow> aggregateByUserId = new HashMap<>();

        for (Deal deal : wonDeals) {
            if (!pipelineAccessService.canAccessDeal(deal, dealScope)) {
                continue;
            }
            User owner = resolveOwnerFromDeal(deal);
            if (owner == null || owner.getId() == null) {
                continue;
            }
            Long userId = owner.getId();
            AdminDashboardDtos.SalesUserWonDealsRow row = aggregateByUserId.computeIfAbsent(userId, id -> {
                AdminDashboardDtos.SalesUserWonDealsRow r = new AdminDashboardDtos.SalesUserWonDealsRow();
                r.userId = id;
                r.userName = (owner.getFirstName() != null ? owner.getFirstName() : "") + " "
                        + (owner.getLastName() != null ? owner.getLastName() : "");
                r.userName = r.userName.trim();
                r.email = owner.getEmail();
                r.totalDeals = 0L;
                r.totalDealValue = BigDecimal.ZERO;
                return r;
            });
            row.totalDeals = row.totalDeals + 1;
            row.totalDealValue = row.totalDealValue.add(deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO);
        }

        AdminDashboardDtos.WonDealsBySalesUserResponse response = new AdminDashboardDtos.WonDealsBySalesUserResponse();
        response.users = new ArrayList<>(aggregateByUserId.values());
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.MonthlyWonDealsBySalesUserResponse getWonDealsBySalesUserMonthly(
            User categoryManager,
            Integer year,
            String category,
            LocalDate dateFrom,
            LocalDate dateTo) {
        if (year == null) {
            throw new IllegalArgumentException("year is required");
        }
        DealAccessScope dealScope = pipelineAccessService.resolveDealAccessScopeForUser(categoryManager);

        Organization.OrganizationCategory categoryFilter = null;
        if (category != null && !category.trim().isEmpty()) {
            categoryFilter = Organization.OrganizationCategory.fromDbValue(category);
        }

        List<Deal> wonDeals = dealRepository.findByStatusAndIsDeletedFalse(DealStatus.WON);

        // userId -> (month -> aggregate)
        Map<Long, Map<Integer, MonthlyAggregate>> aggregates = new HashMap<>();
        Map<Long, User> usersById = new HashMap<>();

        for (Deal deal : wonDeals) {
            // Use won_at (when deal was marked WON); fallback to updated_at/created_at for legacy data
            LocalDateTime reference = deal.getWonAt() != null ? deal.getWonAt()
                    : (deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt());
            if (reference == null || reference.getYear() != year) {
                continue;
            }

            // Optional date range filter (within the same year)
            LocalDate referenceDate = reference.toLocalDate();
            if (dateFrom != null && referenceDate.isBefore(dateFrom)) {
                continue;
            }
            if (dateTo != null && referenceDate.isAfter(dateTo)) {
                continue;
            }

            if (!pipelineAccessService.canAccessDeal(deal, dealScope)) {
                continue;
            }
            User owner = resolveOwnerFromDeal(deal);
            if (owner == null || owner.getId() == null) {
                continue;
            }
            if (owner.getRole() == null || owner.getRole().getName() != Role.RoleName.SALES) {
                continue;
            }

            // Optional organization category filter
            if (categoryFilter != null) {
                Organization organization = deal.getPipeline() != null
                        ? deal.getPipeline().getOrganization()
                        : null;
                if (organization == null || organization.getCategory() == null) {
                    continue;
                }
                if (organization.getCategory() != categoryFilter) {
                    continue;
                }
            }

            Long userId = owner.getId();
            int month = reference.getMonthValue(); // 1-12

            usersById.put(userId, owner);

            Map<Integer, MonthlyAggregate> byMonth =
                    aggregates.computeIfAbsent(userId, id -> new HashMap<>());
            MonthlyAggregate agg =
                    byMonth.computeIfAbsent(month, m -> new MonthlyAggregate());

            agg.totalDeals = agg.totalDeals + 1;
            BigDecimal dealValue = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;
            agg.totalDealValue = agg.totalDealValue.add(dealValue);
            BigDecimal commission = deal.getCommissionAmount() != null
                    ? deal.getCommissionAmount()
                    : BigDecimal.ZERO;
            agg.totalDealCommission = agg.totalDealCommission.add(commission);
        }

        List<AdminDashboardDtos.SalesUserMonthlyWonDealsRow> userRows = new ArrayList<>();
        for (Map.Entry<Long, Map<Integer, MonthlyAggregate>> entry : aggregates.entrySet()) {
            Long userId = entry.getKey();
            Map<Integer, MonthlyAggregate> byMonth = entry.getValue();
            User user = usersById.get(userId);

            AdminDashboardDtos.SalesUserMonthlyWonDealsRow row =
                    new AdminDashboardDtos.SalesUserMonthlyWonDealsRow();
            row.userId = userId;
            if (user != null) {
                String firstName = user.getFirstName() != null ? user.getFirstName() : "";
                String lastName = user.getLastName() != null ? user.getLastName() : "";
                row.userName = (firstName + " " + lastName).trim();
                row.email = user.getEmail();
            }

            List<AdminDashboardDtos.MonthlyDealStats> monthStats = new ArrayList<>();
            for (int m = 1; m <= 12; m++) {
                MonthlyAggregate agg = byMonth.get(m);
                AdminDashboardDtos.MonthlyDealStats stats =
                        new AdminDashboardDtos.MonthlyDealStats();
                stats.month = m;
                if (agg != null) {
                    stats.totalDeals = agg.totalDeals;
                    stats.totalDealValue = agg.totalDealValue;
                    stats.totalDealCommission = agg.totalDealCommission;
                } else {
                    stats.totalDeals = 0L;
                    stats.totalDealValue = BigDecimal.ZERO;
                    stats.totalDealCommission = BigDecimal.ZERO;
                }
                monthStats.add(stats);
            }
            row.months = monthStats;
            userRows.add(row);
        }

        AdminDashboardDtos.MonthlyWonDealsBySalesUserResponse response =
                new AdminDashboardDtos.MonthlyWonDealsBySalesUserResponse();
        response.year = year;
        response.category = categoryFilter != null ? categoryFilter.getDbValue() : null;
        response.users = userRows;
        return response;
    }
    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.LostDealsBySalesUserResponse getLostDealsBySalesUser(User categoryManager) {
        DealAccessScope dealScope = pipelineAccessService.resolveDealAccessScopeForUser(categoryManager);
        List<Deal> lostDeals = dealRepository.findByStatusAndIsDeletedFalse(DealStatus.LOST);
        Map<Long, AdminDashboardDtos.SalesUserLostDealsRow> aggregateByUserId = new HashMap<>();

        for (Deal deal : lostDeals) {
            if (!pipelineAccessService.canAccessDeal(deal, dealScope)) {
                continue;
            }
            User owner = resolveOwnerFromDeal(deal);
            if (owner == null || owner.getId() == null) {
                continue;
            }
            Long userId = owner.getId();
            AdminDashboardDtos.SalesUserLostDealsRow row = aggregateByUserId.computeIfAbsent(userId, id -> {
                AdminDashboardDtos.SalesUserLostDealsRow r = new AdminDashboardDtos.SalesUserLostDealsRow();
                r.userId = id;
                r.userName = (owner.getFirstName() != null ? owner.getFirstName() : "") + " "
                        + (owner.getLastName() != null ? owner.getLastName() : "");
                r.userName = r.userName.trim();
                r.email = owner.getEmail();
                r.totalDeals = 0L;
                r.totalDealValue = BigDecimal.ZERO;
                return r;
            });
            row.totalDeals = row.totalDeals + 1;
            row.totalDealValue = row.totalDealValue.add(deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO);
        }

        AdminDashboardDtos.LostDealsBySalesUserResponse response = new AdminDashboardDtos.LostDealsBySalesUserResponse();
        response.users = new ArrayList<>(aggregateByUserId.values());
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.DealStatusMonthlySummaryResponse getDealStatusMonthlySummary(
            User categoryManager, Integer year) {
        if (year == null) {
            throw new IllegalArgumentException("year is required");
        }
        DealAccessScope dealScope = pipelineAccessService.resolveDealAccessScopeForUser(categoryManager);
        List<Deal> deals = dealRepository.findByIsDeletedFalse();
        Map<Integer, StatusMonthlyAggregate> aggregates = new HashMap<>();

        for (Deal deal : deals) {
            if (deal.getStatus() == null) {
                continue;
            }
            if (!pipelineAccessService.canAccessDeal(deal, dealScope)) {
                continue;
            }
            User owner = resolveOwnerFromDeal(deal);
            LocalDateTime reference = getReferenceDateForStatus(deal);
            if (reference == null || reference.getYear() != year) {
                continue;
            }
            int month = reference.getMonthValue();
            StatusMonthlyAggregate agg = aggregates.computeIfAbsent(month, m -> new StatusMonthlyAggregate());
            BigDecimal value = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;

            String categoryKey = deal.getOrganization() != null && deal.getOrganization().getCategory() != null
                    ? deal.getOrganization().getCategory().getDbValue() : null;
            if (categoryKey != null) {
                agg.byCategory.computeIfAbsent(categoryKey, k -> new StatusByCategory()).add(deal.getStatus(), value);
            }
            agg.byUser.computeIfAbsent(owner.getId(), id -> new StatusByUser(owner)).add(deal.getStatus(), value);
            if (deal.getPipeline() != null && deal.getPipeline().getId() != null) {
                Long pipelineId = deal.getPipeline().getId();
                agg.byPipeline.computeIfAbsent(pipelineId, id -> new StatusByPipeline(deal)).add(deal.getStatus(), value);
            }
            agg.add(deal.getStatus(), value);
        }

        List<AdminDashboardDtos.DealStatusMonthlyRow> monthRows = new ArrayList<>();
        for (int m = 1; m <= 12; m++) {
            StatusMonthlyAggregate agg = aggregates.get(m);
            AdminDashboardDtos.DealStatusMonthlyRow row = new AdminDashboardDtos.DealStatusMonthlyRow();
            row.month = m;
            if (agg != null) {
                row.wonCount = agg.wonCount;
                row.wonValue = agg.wonValue;
                row.lostCount = agg.lostCount;
                row.lostValue = agg.lostValue;
                row.inProgressCount = agg.inProgressCount;
                row.inProgressValue = agg.inProgressValue;
                row.categories = agg.byCategory.entrySet().stream().map(e -> e.getValue().toRow(e.getKey())).collect(Collectors.toList());
                row.users = agg.byUser.values().stream().map(StatusByUser::toRow).collect(Collectors.toList());
                row.pipelines = agg.byPipeline.values().stream().map(StatusByPipeline::toRow).collect(Collectors.toList());
            } else {
                row.wonCount = 0L;
                row.wonValue = BigDecimal.ZERO;
                row.lostCount = 0L;
                row.lostValue = BigDecimal.ZERO;
                row.inProgressCount = 0L;
                row.inProgressValue = BigDecimal.ZERO;
                row.categories = new ArrayList<>();
                row.users = new ArrayList<>();
                row.pipelines = new ArrayList<>();
            }
            monthRows.add(row);
        }
        AdminDashboardDtos.DealStatusMonthlySummaryResponse response =
                new AdminDashboardDtos.DealStatusMonthlySummaryResponse();
        response.year = year;
        response.months = monthRows;
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.DealStatusMonthlyByUserResponse getDealStatusMonthlyByUser(
            User categoryManager, Integer year) {
        if (year == null) {
            throw new IllegalArgumentException("year is required");
        }
        DealAccessScope dealScope = pipelineAccessService.resolveDealAccessScopeForUser(categoryManager);
        List<Deal> deals = dealRepository.findByIsDeletedFalse();
        // userId -> (month -> aggregate for that month)
        Map<Long, Map<Integer, StatusByUser>> byUserByMonth = new HashMap<>();
        Map<Long, User> userIdToUser = new HashMap<>();

        for (Deal deal : deals) {
            if (deal.getStatus() == null) {
                continue;
            }
            if (!pipelineAccessService.canAccessDeal(deal, dealScope)) {
                continue;
            }
            User owner = resolveOwnerFromDeal(deal);
            if (owner == null || owner.getId() == null) {
                continue;
            }
            LocalDateTime reference = getReferenceDateForStatus(deal);
            if (reference == null || reference.getYear() != year) {
                continue;
            }
            int month = reference.getMonthValue();
            userIdToUser.put(owner.getId(), owner);
            Map<Integer, StatusByUser> byMonth = byUserByMonth.computeIfAbsent(owner.getId(), id -> new HashMap<>());
            StatusByUser agg = byMonth.computeIfAbsent(month, m -> new StatusByUser(owner));
            BigDecimal value = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;
            agg.add(deal.getStatus(), value);
        }

        List<AdminDashboardDtos.DealStatusMonthlyByUserRow> userRows = new ArrayList<>();
        for (Map.Entry<Long, Map<Integer, StatusByUser>> entry : byUserByMonth.entrySet()) {
            Long uid = entry.getKey();
            Map<Integer, StatusByUser> monthAggregates = entry.getValue();
            User user = userIdToUser.get(uid);
            AdminDashboardDtos.DealStatusMonthlyByUserRow userRow = new AdminDashboardDtos.DealStatusMonthlyByUserRow();
            userRow.userId = uid;
            userRow.userName = user != null
                    ? ((user.getFirstName() != null ? user.getFirstName() : "") + " "
                    + (user.getLastName() != null ? user.getLastName() : "")).trim()
                    : null;
            userRow.email = user != null ? user.getEmail() : null;
            userRow.months = new ArrayList<>();
            for (int m = 1; m <= 12; m++) {
                AdminDashboardDtos.DealStatusMonthlyUserMonthRow monthRow =
                        new AdminDashboardDtos.DealStatusMonthlyUserMonthRow();
                monthRow.month = m;
                StatusByUser agg = monthAggregates.get(m);
                if (agg != null) {
                    monthRow.wonCount = agg.wonCount;
                    monthRow.wonValue = agg.wonValue;
                    monthRow.lostCount = agg.lostCount;
                    monthRow.lostValue = agg.lostValue;
                    monthRow.inProgressCount = agg.inProgressCount;
                    monthRow.inProgressValue = agg.inProgressValue;
                } else {
                    monthRow.wonCount = 0L;
                    monthRow.wonValue = BigDecimal.ZERO;
                    monthRow.lostCount = 0L;
                    monthRow.lostValue = BigDecimal.ZERO;
                    monthRow.inProgressCount = 0L;
                    monthRow.inProgressValue = BigDecimal.ZERO;
                }
                userRow.months.add(monthRow);
            }
            userRows.add(userRow);
        }

        AdminDashboardDtos.DealStatusMonthlyByUserResponse response =
                new AdminDashboardDtos.DealStatusMonthlyByUserResponse();
        response.year = year;
        response.users = userRows;
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.DealStatusMonthlyByPipelineResponse getDealStatusMonthlyByPipeline(
            User categoryManager, Integer year) {
        if (year == null) {
            throw new IllegalArgumentException("year is required");
        }
        DealAccessScope dealScope = pipelineAccessService.resolveDealAccessScopeForUser(categoryManager);
        List<Deal> deals = dealRepository.findByIsDeletedFalse();
        // pipelineId -> (month -> aggregate for that month)
        Map<Long, Map<Integer, StatusByPipeline>> byPipelineByMonth = new HashMap<>();

        for (Deal deal : deals) {
            if (deal.getStatus() == null || deal.getPipeline() == null || deal.getPipeline().getId() == null) {
                continue;
            }
            if (!pipelineAccessService.canAccessDeal(deal, dealScope)) {
                continue;
            }
            LocalDateTime reference = getReferenceDateForStatus(deal);
            if (reference == null || reference.getYear() != year) {
                continue;
            }
            int month = reference.getMonthValue();
            Long pipelineId = deal.getPipeline().getId();
            Map<Integer, StatusByPipeline> byMonth = byPipelineByMonth.computeIfAbsent(pipelineId, id -> new HashMap<>());
            StatusByPipeline agg = byMonth.computeIfAbsent(month, m -> new StatusByPipeline(deal));
            BigDecimal value = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;
            agg.add(deal.getStatus(), value);
        }

        List<AdminDashboardDtos.DealStatusMonthlyByPipelineRow> pipelineRows = new ArrayList<>();
        for (Map.Entry<Long, Map<Integer, StatusByPipeline>> entry : byPipelineByMonth.entrySet()) {
            Long pipelineId = entry.getKey();
            Map<Integer, StatusByPipeline> monthAggregates = entry.getValue();
            String pipelineName = monthAggregates.isEmpty() ? null
                    : monthAggregates.values().iterator().next().pipelineName;
            AdminDashboardDtos.DealStatusMonthlyByPipelineRow pipelineRow =
                    new AdminDashboardDtos.DealStatusMonthlyByPipelineRow();
            pipelineRow.pipelineId = pipelineId;
            pipelineRow.pipelineName = pipelineName;
            pipelineRow.months = new ArrayList<>();
            for (int m = 1; m <= 12; m++) {
                AdminDashboardDtos.DealStatusMonthlyUserMonthRow monthRow =
                        new AdminDashboardDtos.DealStatusMonthlyUserMonthRow();
                monthRow.month = m;
                StatusByPipeline agg = monthAggregates.get(m);
                if (agg != null) {
                    monthRow.wonCount = agg.wonCount;
                    monthRow.wonValue = agg.wonValue;
                    monthRow.lostCount = agg.lostCount;
                    monthRow.lostValue = agg.lostValue;
                    monthRow.inProgressCount = agg.inProgressCount;
                    monthRow.inProgressValue = agg.inProgressValue;
                } else {
                    monthRow.wonCount = 0L;
                    monthRow.wonValue = BigDecimal.ZERO;
                    monthRow.lostCount = 0L;
                    monthRow.lostValue = BigDecimal.ZERO;
                    monthRow.inProgressCount = 0L;
                    monthRow.inProgressValue = BigDecimal.ZERO;
                }
                pipelineRow.months.add(monthRow);
            }
            pipelineRows.add(pipelineRow);
        }

        AdminDashboardDtos.DealStatusMonthlyByPipelineResponse response =
                new AdminDashboardDtos.DealStatusMonthlyByPipelineResponse();
        response.year = year;
        response.pipelines = pipelineRows;
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.LostReasonSummaryResponse getLostReasonSummary(User categoryManager,
            String category, Long userId, Long pipelineId) {
        DealAccessScope dealScope = pipelineAccessService.resolveDealAccessScopeForUser(categoryManager);
        Organization.OrganizationCategory categoryFilter = null;
        if (category != null && !category.trim().isEmpty()) {
            categoryFilter = Organization.OrganizationCategory.fromDbValue(category);
        }
        List<Deal> lostDeals = dealRepository.findByStatusAndIsDeletedFalse(DealStatus.LOST);
        Map<String, Long> countsByReason = new HashMap<>();
        long totalLost = 0L;

        for (Deal deal : lostDeals) {
            if (deal.getLostReason() == null) {
                continue;
            }
            if (!pipelineAccessService.canAccessDeal(deal, dealScope)) {
                continue;
            }
            User owner = resolveOwnerFromDeal(deal);
            if (categoryFilter != null) {
                Organization org = deal.getOrganization();
                if (org == null || org.getCategory() == null || org.getCategory() != categoryFilter) {
                    continue;
                }
            }
            if (userId != null && (owner == null || owner.getId() == null || !userId.equals(owner.getId()))) {
                continue;
            }
            if (pipelineId != null && (deal.getPipeline() == null || deal.getPipeline().getId() == null
                    || !pipelineId.equals(deal.getPipeline().getId()))) {
                continue;
            }
            String reasonLabel = deal.getLostReason().toDisplayString();
            countsByReason.merge(reasonLabel, 1L, Long::sum);
            totalLost++;
        }

        List<AdminDashboardDtos.LostReasonRow> rows = new ArrayList<>();
        for (Map.Entry<String, Long> entry : countsByReason.entrySet()) {
            AdminDashboardDtos.LostReasonRow row = new AdminDashboardDtos.LostReasonRow();
            row.reason = entry.getKey();
            row.count = entry.getValue();
            row.percentage = totalLost > 0
                    ? BigDecimal.valueOf(entry.getValue() * 100.0).divide(BigDecimal.valueOf(totalLost), 2, java.math.RoundingMode.HALF_UP)
                    : BigDecimal.ZERO;
            rows.add(row);
        }
        AdminDashboardDtos.LostReasonSummaryResponse response = new AdminDashboardDtos.LostReasonSummaryResponse();
        response.totalLostDeals = totalLost;
        response.category = categoryFilter != null ? categoryFilter.getDbValue() : null;
        response.userId = userId;
        response.pipelineId = pipelineId;
        response.reasons = rows;
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.LostReasonsByPipelineResponse getLostReasonsByPipeline(
            User categoryManager, String category) {
        DealAccessScope dealScope = pipelineAccessService.resolveDealAccessScopeForUser(categoryManager);
        Organization.OrganizationCategory categoryFilter = null;
        if (category != null && !category.trim().isEmpty()) {
            categoryFilter = Organization.OrganizationCategory.fromDbValue(category);
        }
        List<Deal> lostDeals = dealRepository.findByStatusAndIsDeletedFalse(DealStatus.LOST);
        // pipelineId -> (reasonLabel -> count)
        Map<Long, Map<String, Long>> byPipeline = new HashMap<>();
        Map<Long, String> pipelineNames = new HashMap<>();
        Map<Long, Long> pipelineTotals = new HashMap<>();

        for (Deal deal : lostDeals) {
            if (deal.getLostReason() == null || deal.getPipeline() == null || deal.getPipeline().getId() == null) {
                continue;
            }
            if (!pipelineAccessService.canAccessDeal(deal, dealScope)) {
                continue;
            }
            if (categoryFilter != null) {
                Organization org = deal.getOrganization();
                if (org == null || org.getCategory() == null || org.getCategory() != categoryFilter) {
                    continue;
                }
            }
            Long pipelineId = deal.getPipeline().getId();
            pipelineNames.put(pipelineId, deal.getPipeline().getName());
            Map<String, Long> reasons = byPipeline.computeIfAbsent(pipelineId, id -> new HashMap<>());
            String reasonLabel = deal.getLostReason().toDisplayString();
            reasons.merge(reasonLabel, 1L, Long::sum);
            pipelineTotals.merge(pipelineId, 1L, Long::sum);
        }

        List<AdminDashboardDtos.LostReasonsByPipelineRow> rows = new ArrayList<>();
        for (Map.Entry<Long, Map<String, Long>> entry : byPipeline.entrySet()) {
            Long pipelineId = entry.getKey();
            long total = pipelineTotals.getOrDefault(pipelineId, 0L);
            List<AdminDashboardDtos.LostReasonRow> reasonRows = new ArrayList<>();
            for (Map.Entry<String, Long> re : entry.getValue().entrySet()) {
                AdminDashboardDtos.LostReasonRow r = new AdminDashboardDtos.LostReasonRow();
                r.reason = re.getKey();
                r.count = re.getValue();
                r.percentage = total > 0
                        ? BigDecimal.valueOf(re.getValue() * 100.0).divide(BigDecimal.valueOf(total), 2, java.math.RoundingMode.HALF_UP)
                        : BigDecimal.ZERO;
                reasonRows.add(r);
            }
            AdminDashboardDtos.LostReasonsByPipelineRow row = new AdminDashboardDtos.LostReasonsByPipelineRow();
            row.pipelineId = pipelineId;
            row.pipelineName = pipelineNames.get(pipelineId);
            row.totalLostDeals = total;
            row.reasons = reasonRows;
            rows.add(row);
        }

        AdminDashboardDtos.LostReasonsByPipelineResponse response = new AdminDashboardDtos.LostReasonsByPipelineResponse();
        response.category = categoryFilter != null ? categoryFilter.getDbValue() : null;
        response.pipelines = rows;
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.LostReasonsByUserResponse getLostReasonsByUser(
            User categoryManager, String category) {
        DealAccessScope dealScope = pipelineAccessService.resolveDealAccessScopeForUser(categoryManager);
        Organization.OrganizationCategory categoryFilter = null;
        if (category != null && !category.trim().isEmpty()) {
            categoryFilter = Organization.OrganizationCategory.fromDbValue(category);
        }
        List<Deal> lostDeals = dealRepository.findByStatusAndIsDeletedFalse(DealStatus.LOST);
        // userId -> (reasonLabel -> count)
        Map<Long, Map<String, Long>> byUser = new HashMap<>();
        Map<Long, User> userById = new HashMap<>();
        Map<Long, Long> userTotals = new HashMap<>();

        for (Deal deal : lostDeals) {
            if (deal.getLostReason() == null) {
                continue;
            }
            if (!pipelineAccessService.canAccessDeal(deal, dealScope)) {
                continue;
            }
            User owner = resolveOwnerFromDeal(deal);
            if (owner == null || owner.getId() == null) {
                continue;
            }
            if (categoryFilter != null) {
                Organization org = deal.getOrganization();
                if (org == null || org.getCategory() == null || org.getCategory() != categoryFilter) {
                    continue;
                }
            }
            Long userId = owner.getId();
            userById.put(userId, owner);
            Map<String, Long> reasons = byUser.computeIfAbsent(userId, id -> new HashMap<>());
            String reasonLabel = deal.getLostReason().toDisplayString();
            reasons.merge(reasonLabel, 1L, Long::sum);
            userTotals.merge(userId, 1L, Long::sum);
        }

        List<AdminDashboardDtos.LostReasonsByUserRow> rows = new ArrayList<>();
        for (Map.Entry<Long, Map<String, Long>> entry : byUser.entrySet()) {
            Long userId = entry.getKey();
            long total = userTotals.getOrDefault(userId, 0L);
            User user = userById.get(userId);
            List<AdminDashboardDtos.LostReasonRow> reasonRows = new ArrayList<>();
            for (Map.Entry<String, Long> re : entry.getValue().entrySet()) {
                AdminDashboardDtos.LostReasonRow r = new AdminDashboardDtos.LostReasonRow();
                r.reason = re.getKey();
                r.count = re.getValue();
                r.percentage = total > 0
                        ? BigDecimal.valueOf(re.getValue() * 100.0).divide(BigDecimal.valueOf(total), 2, java.math.RoundingMode.HALF_UP)
                        : BigDecimal.ZERO;
                reasonRows.add(r);
            }
            AdminDashboardDtos.LostReasonsByUserRow row = new AdminDashboardDtos.LostReasonsByUserRow();
            row.userId = userId;
            row.userName = user != null
                    ? ((user.getFirstName() != null ? user.getFirstName() : "") + " "
                    + (user.getLastName() != null ? user.getLastName() : "")).trim()
                    : null;
            row.email = user != null ? user.getEmail() : null;
            row.totalLostDeals = total;
            row.reasons = reasonRows;
            rows.add(row);
        }

        AdminDashboardDtos.LostReasonsByUserResponse response = new AdminDashboardDtos.LostReasonsByUserResponse();
        response.category = categoryFilter != null ? categoryFilter.getDbValue() : null;
        response.users = rows;
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.SalesPipelinesResponse getSalesUsersWithPipelines(User categoryManager) {
        DealAccessScope dealScope = pipelineAccessService.resolveDealAccessScopeForUser(categoryManager);
        // Fetch all active pipelines
        java.util.List<com.brideside.crm.entity.Pipeline> pipelines =
                pipelineRepository.findByDeletedFalseOrderByNameAsc();

        // userId -> (pipelineId -> pipelineName)
        Map<Long, Map<Long, String>> pipelinesByUser = new HashMap<>();
        Map<Long, User> usersById = new HashMap<>();

        for (com.brideside.crm.entity.Pipeline pipeline : pipelines) {
            if (pipeline.getOrganization() == null || pipeline.getOrganization().getOwner() == null) {
                continue;
            }
            User owner = pipeline.getOrganization().getOwner();
            if (!pipelineAccessService.canAccessPipeline(pipeline.getId(), dealScope)) {
                continue;
            }
            if (owner.getRole() == null || owner.getRole().getName() != Role.RoleName.SALES) {
                continue;
            }
            Long userId = owner.getId();
            usersById.put(userId, owner);
            Map<Long, String> userPipelines =
                    pipelinesByUser.computeIfAbsent(userId, id -> new java.util.LinkedHashMap<>());
            userPipelines.putIfAbsent(pipeline.getId(), pipeline.getName());
        }

        java.util.List<AdminDashboardDtos.SalesPipelinesRow> userRows = new ArrayList<>();
        for (Map.Entry<Long, Map<Long, String>> entry : pipelinesByUser.entrySet()) {
            Long userId = entry.getKey();
            User user = usersById.get(userId);
            AdminDashboardDtos.SalesPipelinesRow row = new AdminDashboardDtos.SalesPipelinesRow();
            row.userId = userId;
            row.userName = user != null
                    ? ((user.getFirstName() != null ? user.getFirstName() : "") + " "
                    + (user.getLastName() != null ? user.getLastName() : "")).trim()
                    : null;
            row.email = user != null ? user.getEmail() : null;
            row.pipelines = new ArrayList<>();
            for (Map.Entry<Long, String> pe : entry.getValue().entrySet()) {
                AdminDashboardDtos.PipelineRef pref = new AdminDashboardDtos.PipelineRef();
                pref.pipelineId = pe.getKey();
                pref.pipelineName = pe.getValue();
                row.pipelines.add(pref);
            }
            userRows.add(row);
        }

        AdminDashboardDtos.SalesPipelinesResponse response = new AdminDashboardDtos.SalesPipelinesResponse();
        response.users = userRows;
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.UserActivityMonthlySummaryResponse getUserActivityMonthlySummary(
            User categoryManager, Integer year) {
        if (year == null) {
            throw new IllegalArgumentException("year is required");
        }

        Set<Long> allowedUserIds = getAllowedOwnerIdsForCategoryManager(categoryManager);
        List<Activity> activities = activityRepository.findAll();

        // userId -> (month -> aggregate)
        Map<Long, Map<Integer, ActivityMonthlyAggregate>> aggregates = new HashMap<>();

        for (Activity activity : activities) {
            Long userId = activity.getAssignedUserId();
            if (userId == null || !allowedUserIds.contains(userId)) {
                continue;
            }
            if (activity.getCreatedAt() == null) {
                continue;
            }

            LocalDateTime reference = LocalDateTime.ofInstant(
                    activity.getCreatedAt(),
                    ZoneId.systemDefault()
            );
            if (reference.getYear() != year) {
                continue;
            }

            int month = reference.getMonthValue(); // 1-12

            Map<Integer, ActivityMonthlyAggregate> byMonth =
                    aggregates.computeIfAbsent(userId, id -> new HashMap<>());
            ActivityMonthlyAggregate agg =
                    byMonth.computeIfAbsent(month, m -> new ActivityMonthlyAggregate());

            agg.totalActivities = agg.totalActivities + 1;

            if (activity.getCategory() == Activity.ActivityCategory.CALL) {
                agg.callCount = agg.callCount + 1;
                int minutes = activity.getDurationMinutes() != null
                        ? activity.getDurationMinutes()
                        : 0;
                agg.totalCallMinutes = agg.totalCallMinutes + minutes;
            }

            if (activity.getCategory() == Activity.ActivityCategory.MEETING
                    || activity.getCategory() == Activity.ActivityCategory.MEETING_SCHEDULER) {
                agg.meetingCount = agg.meetingCount + 1;
                int minutes = activity.getDurationMinutes() != null
                        ? activity.getDurationMinutes()
                        : 0;
                agg.totalMeetingMinutes = agg.totalMeetingMinutes + minutes;
            }
        }

        // Preload users for names/emails
        Map<Long, User> usersById = new HashMap<>();
        if (!aggregates.isEmpty()) {
            usersById.putAll(
                    userRepository.findAllById(aggregates.keySet()).stream()
                            .collect(Collectors.toMap(
                                    User::getId,
                                    u -> u
                            ))
            );
        }

        List<AdminDashboardDtos.UserActivityMonthlyRow> userRows = new ArrayList<>();
        for (Map.Entry<Long, Map<Integer, ActivityMonthlyAggregate>> entry : aggregates.entrySet()) {
            Long userId = entry.getKey();
            Map<Integer, ActivityMonthlyAggregate> byMonth = entry.getValue();

            AdminDashboardDtos.UserActivityMonthlyRow row =
                    new AdminDashboardDtos.UserActivityMonthlyRow();
            row.userId = userId;

            User user = usersById.get(userId);
            if (user != null) {
                String firstName = user.getFirstName() != null ? user.getFirstName() : "";
                String lastName = user.getLastName() != null ? user.getLastName() : "";
                row.userName = (firstName + " " + lastName).trim();
                row.email = user.getEmail();
            } else {
                row.userName = null;
                row.email = null;
            }

            List<AdminDashboardDtos.UserActivityMonthStats> monthStats = new ArrayList<>();
            for (int m = 1; m <= 12; m++) {
                ActivityMonthlyAggregate agg = byMonth.get(m);
                AdminDashboardDtos.UserActivityMonthStats stats =
                        new AdminDashboardDtos.UserActivityMonthStats();
                stats.month = m;
                if (agg != null) {
                    stats.totalActivities = agg.totalActivities;
                    stats.callCount = agg.callCount;
                    stats.totalCallMinutes = agg.totalCallMinutes;
                    stats.meetingCount = agg.meetingCount;
                    stats.totalMeetingMinutes = agg.totalMeetingMinutes;
                } else {
                    stats.totalActivities = 0L;
                    stats.callCount = 0L;
                    stats.totalCallMinutes = 0L;
                    stats.meetingCount = 0L;
                    stats.totalMeetingMinutes = 0L;
                }
                monthStats.add(stats);
            }
            row.months = monthStats;
            userRows.add(row);
        }

        AdminDashboardDtos.UserActivityMonthlySummaryResponse response =
                new AdminDashboardDtos.UserActivityMonthlySummaryResponse();
        response.year = year;
        response.users = userRows;
        return response;
    }

    private Set<Long> getAllowedOwnerIdsForCategoryManager(User categoryManager) {
        Set<Long> ownerIds = new HashSet<>();
        ownerIds.add(categoryManager.getId());
        List<User> directReports = userRepository.findByManagerId(categoryManager.getId());
        for (User report : directReports) {
            if (report.getRole() != null) {
                Role.RoleName rn = report.getRole().getName();
                if (rn == Role.RoleName.SALES || rn == Role.RoleName.PRESALES) {
                    ownerIds.add(report.getId());
                }
            }
        }
        for (User sales : directReports) {
            if (sales.getRole() != null && sales.getRole().getName() == Role.RoleName.SALES) {
                for (User presales : userRepository.findByManagerId(sales.getId())) {
                    if (presales.getRole() != null && presales.getRole().getName() == Role.RoleName.PRESALES) {
                        ownerIds.add(presales.getId());
                    }
                }
            }
        }
        return ownerIds;
    }

    /** Resolve organization owner from deal (SALES or CATEGORY_MANAGER only). */
    private User resolveOwnerFromDeal(Deal deal) {
        if (deal == null || deal.getPipeline() == null) {
            return null;
        }
        Organization organization = deal.getPipeline().getOrganization();
        if (organization == null) {
            return null;
        }
        User owner = organization.getOwner();
        if (owner == null || owner.getId() == null || owner.getRole() == null) {
            return null;
        }
        Role.RoleName name = owner.getRole().getName();
        if (name != Role.RoleName.SALES && name != Role.RoleName.CATEGORY_MANAGER) {
            return null;
        }
        return owner;
    }

    private static LocalDateTime getReferenceDateForStatus(Deal deal) {
        if (deal.getStatus() == DealStatus.WON) {
            return deal.getWonAt() != null ? deal.getWonAt()
                    : (deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt());
        }
        if (deal.getStatus() == DealStatus.LOST) {
            return deal.getLostAt() != null ? deal.getLostAt()
                    : (deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt());
        }
        return deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt();
    }

    // --- Inner aggregates for revenue ---
    private static class CategoryRevenueAggregate {
        long totalDeals = 0L;
        BigDecimal totalValue = BigDecimal.ZERO;
        void add(BigDecimal value) {
            totalDeals++;
            totalValue = totalValue.add(value);
        }
    }

    private static class UserRevenueAggregate {
        final Long userId;
        final String userName;
        final String email;
        long totalDeals = 0L;
        BigDecimal totalValue = BigDecimal.ZERO;
        UserRevenueAggregate(User owner) {
            this.userId = owner.getId();
            this.userName = ((owner.getFirstName() != null ? owner.getFirstName() : "") + " "
                    + (owner.getLastName() != null ? owner.getLastName() : "")).trim();
            this.email = owner.getEmail();
        }
        void add(BigDecimal value) {
            totalDeals++;
            totalValue = totalValue.add(value);
        }
    }

    private static class PipelineRevenueAggregate {
        final Long pipelineId;
        final String pipelineName;
        long totalDeals = 0L;
        BigDecimal totalValue = BigDecimal.ZERO;
        PipelineRevenueAggregate(Deal deal) {
            this.pipelineId = deal.getPipeline() != null ? deal.getPipeline().getId() : null;
            this.pipelineName = deal.getPipeline() != null ? deal.getPipeline().getName() : null;
        }
        void add(BigDecimal value) {
            totalDeals++;
            totalValue = totalValue.add(value);
        }
    }

    // --- Monthly status aggregates ---
    private static class StatusMonthlyAggregate {
        long wonCount = 0L;
        BigDecimal wonValue = BigDecimal.ZERO;
        long lostCount = 0L;
        BigDecimal lostValue = BigDecimal.ZERO;
        long inProgressCount = 0L;
        BigDecimal inProgressValue = BigDecimal.ZERO;
        Map<String, StatusByCategory> byCategory = new HashMap<>();
        Map<Long, StatusByUser> byUser = new HashMap<>();
        Map<Long, StatusByPipeline> byPipeline = new HashMap<>();
        void add(DealStatus status, BigDecimal value) {
            if (status == DealStatus.WON) {
                wonCount++;
                wonValue = wonValue.add(value);
            } else if (status == DealStatus.LOST) {
                lostCount++;
                lostValue = lostValue.add(value);
            } else if (status == DealStatus.IN_PROGRESS) {
                inProgressCount++;
                inProgressValue = inProgressValue.add(value);
            }
        }
    }

    private static class StatusByCategory {
        long wonCount = 0L;
        BigDecimal wonValue = BigDecimal.ZERO;
        long lostCount = 0L;
        BigDecimal lostValue = BigDecimal.ZERO;
        long inProgressCount = 0L;
        BigDecimal inProgressValue = BigDecimal.ZERO;
        void add(DealStatus status, BigDecimal value) {
            if (status == DealStatus.WON) {
                wonCount++;
                wonValue = wonValue.add(value);
            } else if (status == DealStatus.LOST) {
                lostCount++;
                lostValue = lostValue.add(value);
            } else if (status == DealStatus.IN_PROGRESS) {
                inProgressCount++;
                inProgressValue = inProgressValue.add(value);
            }
        }
        AdminDashboardDtos.DealStatusCategoryRow toRow(String category) {
            AdminDashboardDtos.DealStatusCategoryRow row = new AdminDashboardDtos.DealStatusCategoryRow();
            row.category = category;
            row.wonCount = wonCount;
            row.wonValue = wonValue;
            row.lostCount = lostCount;
            row.lostValue = lostValue;
            row.inProgressCount = inProgressCount;
            row.inProgressValue = inProgressValue;
            return row;
        }
    }

    private static class StatusByUser {
        final Long userId;
        final String userName;
        final String email;
        long wonCount = 0L;
        BigDecimal wonValue = BigDecimal.ZERO;
        long lostCount = 0L;
        BigDecimal lostValue = BigDecimal.ZERO;
        long inProgressCount = 0L;
        BigDecimal inProgressValue = BigDecimal.ZERO;
        StatusByUser(User owner) {
            this.userId = owner.getId();
            this.userName = ((owner.getFirstName() != null ? owner.getFirstName() : "") + " "
                    + (owner.getLastName() != null ? owner.getLastName() : "")).trim();
            this.email = owner.getEmail();
        }
        void add(DealStatus status, BigDecimal value) {
            if (status == DealStatus.WON) {
                wonCount++;
                wonValue = wonValue.add(value);
            } else if (status == DealStatus.LOST) {
                lostCount++;
                lostValue = lostValue.add(value);
            } else if (status == DealStatus.IN_PROGRESS) {
                inProgressCount++;
                inProgressValue = inProgressValue.add(value);
            }
        }
        AdminDashboardDtos.DealStatusMonthlyUserRow toRow() {
            AdminDashboardDtos.DealStatusMonthlyUserRow row = new AdminDashboardDtos.DealStatusMonthlyUserRow();
            row.userId = userId;
            row.userName = userName;
            row.email = email;
            row.wonCount = wonCount;
            row.wonValue = wonValue;
            row.lostCount = lostCount;
            row.lostValue = lostValue;
            row.inProgressCount = inProgressCount;
            row.inProgressValue = inProgressValue;
            return row;
        }
    }

    private static class StatusByPipeline {
        final Long pipelineId;
        final String pipelineName;
        long wonCount = 0L;
        BigDecimal wonValue = BigDecimal.ZERO;
        long lostCount = 0L;
        BigDecimal lostValue = BigDecimal.ZERO;
        long inProgressCount = 0L;
        BigDecimal inProgressValue = BigDecimal.ZERO;
        StatusByPipeline(Deal deal) {
            this.pipelineId = deal.getPipeline() != null ? deal.getPipeline().getId() : null;
            this.pipelineName = deal.getPipeline() != null ? deal.getPipeline().getName() : null;
        }
        void add(DealStatus status, BigDecimal value) {
            if (status == DealStatus.WON) {
                wonCount++;
                wonValue = wonValue.add(value);
            } else if (status == DealStatus.LOST) {
                lostCount++;
                lostValue = lostValue.add(value);
            } else if (status == DealStatus.IN_PROGRESS) {
                inProgressCount++;
                inProgressValue = inProgressValue.add(value);
            }
        }
        AdminDashboardDtos.DealStatusPipelineRow toRow() {
            AdminDashboardDtos.DealStatusPipelineRow row = new AdminDashboardDtos.DealStatusPipelineRow();
            row.pipelineId = pipelineId;
            row.pipelineName = pipelineName;
            row.wonCount = wonCount;
            row.wonValue = wonValue;
            row.lostCount = lostCount;
            row.lostValue = lostValue;
            row.inProgressCount = inProgressCount;
            row.inProgressValue = inProgressValue;
            return row;
        }
    }

    private static class ActivityMonthlyAggregate {
        Long totalActivities = 0L;
        Long callCount = 0L;
        Long totalCallMinutes = 0L;
        Long meetingCount = 0L;
        Long totalMeetingMinutes = 0L;
    }

    private static class MonthlyAggregate {
        Long totalDeals = 0L;
        BigDecimal totalDealValue = BigDecimal.ZERO;
        BigDecimal totalDealCommission = BigDecimal.ZERO;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.OrganizationDealStatusSummaryResponse getOrganizationDealStatusSummary(
            User categoryManager, Integer year) {
        if (year == null) {
            throw new IllegalArgumentException("year is required");
        }
        DealAccessScope dealScope = pipelineAccessService.resolveDealAccessScopeForUser(categoryManager);
        List<Deal> deals = dealRepository.findByIsDeletedFalse();
        Map<Long, OrganizationAggregate> aggregates = new HashMap<>();

        for (Deal deal : deals) {
            if (!pipelineAccessService.canAccessDeal(deal, dealScope)) {
                continue;
            }
            if (deal.getOrganization() == null || deal.getOrganization().getId() == null) {
                continue;
            }
            Organization org = deal.getOrganization();
            User owner = org.getOwner();

            Long orgId = org.getId();
            String orgName = org.getName();
            OrganizationAggregate agg = aggregates.computeIfAbsent(orgId, id -> {
                String category = null;
                if (org.getCategory() != null) {
                    category = org.getCategory().getDbValue();
                }
                Long ownerId = owner != null ? owner.getId() : null;
                String firstName = owner != null && owner.getFirstName() != null ? owner.getFirstName() : "";
                String lastName = owner != null && owner.getLastName() != null ? owner.getLastName() : "";
                String ownerName = (firstName + " " + lastName).trim();
                String ownerEmail = owner != null ? owner.getEmail() : null;
                return new OrganizationAggregate(orgName, category, ownerId, ownerName, ownerEmail);
            });

            BigDecimal value = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;

            // All-time totals
            if (deal.getStatus() == DealStatus.WON) {
                agg.wonCountAll++;
                agg.wonValueAll = agg.wonValueAll.add(value);
            } else if (deal.getStatus() == DealStatus.LOST) {
                agg.lostCountAll++;
                agg.lostValueAll = agg.lostValueAll.add(value);
            } else if (deal.getStatus() == DealStatus.IN_PROGRESS) {
                agg.inProgressCountAll++;
                agg.inProgressValueAll = agg.inProgressValueAll.add(value);
            }

            // Yearly/monthly breakdown
            if (deal.getStatus() != null) {
                LocalDateTime reference = getReferenceDateForStatus(deal);
                if (reference != null && reference.getYear() == year) {
                    int month = reference.getMonthValue();
                    StatusMonthlyAggregate statusAgg = agg.monthlyStatus.computeIfAbsent(month, m -> new StatusMonthlyAggregate());
                    if (deal.getStatus() == DealStatus.WON) {
                        statusAgg.wonCount++;
                        statusAgg.wonValue = statusAgg.wonValue.add(value);
                    } else if (deal.getStatus() == DealStatus.LOST) {
                        statusAgg.lostCount++;
                        statusAgg.lostValue = statusAgg.lostValue.add(value);
                    } else if (deal.getStatus() == DealStatus.IN_PROGRESS) {
                        statusAgg.inProgressCount++;
                        statusAgg.inProgressValue = statusAgg.inProgressValue.add(value);
                    }
                }
            }
        }

        List<AdminDashboardDtos.OrganizationDealStatusRow> orgRows = new ArrayList<>();
        for (Map.Entry<Long, OrganizationAggregate> entry : aggregates.entrySet()) {
            Long orgId = entry.getKey();
            OrganizationAggregate agg = entry.getValue();
            AdminDashboardDtos.OrganizationDealStatusRow row = new AdminDashboardDtos.OrganizationDealStatusRow();
            row.organizationId = orgId;
            row.organizationName = agg.organizationName;
            row.organizationCategory = agg.organizationCategory;
            row.ownerId = agg.ownerId;
            row.ownerName = agg.ownerName;
            row.ownerEmail = agg.ownerEmail;
            row.wonCountAll = agg.wonCountAll;
            row.wonValueAll = agg.wonValueAll;
            row.lostCountAll = agg.lostCountAll;
            row.lostValueAll = agg.lostValueAll;
            row.inProgressCountAll = agg.inProgressCountAll;
            row.inProgressValueAll = agg.inProgressValueAll;

            List<AdminDashboardDtos.OrganizationDealStatusMonthlyRow> months = new ArrayList<>();
            for (int m = 1; m <= 12; m++) {
                StatusMonthlyAggregate statusAgg = agg.monthlyStatus.get(m);
                AdminDashboardDtos.OrganizationDealStatusMonthlyRow monthRow =
                        new AdminDashboardDtos.OrganizationDealStatusMonthlyRow();
                monthRow.month = m;
                if (statusAgg != null) {
                    monthRow.wonCount = statusAgg.wonCount;
                    monthRow.wonValue = statusAgg.wonValue;
                    monthRow.lostCount = statusAgg.lostCount;
                    monthRow.lostValue = statusAgg.lostValue;
                    monthRow.inProgressCount = statusAgg.inProgressCount;
                    monthRow.inProgressValue = statusAgg.inProgressValue;
                } else {
                    monthRow.wonCount = 0L;
                    monthRow.wonValue = BigDecimal.ZERO;
                    monthRow.lostCount = 0L;
                    monthRow.lostValue = BigDecimal.ZERO;
                    monthRow.inProgressCount = 0L;
                    monthRow.inProgressValue = BigDecimal.ZERO;
                }
                months.add(monthRow);
            }
            row.months = months;
            orgRows.add(row);
        }

        AdminDashboardDtos.OrganizationDealStatusSummaryResponse response =
                new AdminDashboardDtos.OrganizationDealStatusSummaryResponse();
        response.year = year;
        response.organizations = orgRows;
        return response;
    }

    private static class OrganizationAggregate {
        final String organizationName;
        final String organizationCategory;
        final Long ownerId;
        final String ownerName;
        final String ownerEmail;
        long wonCountAll = 0L;
        BigDecimal wonValueAll = BigDecimal.ZERO;
        long lostCountAll = 0L;
        BigDecimal lostValueAll = BigDecimal.ZERO;
        long inProgressCountAll = 0L;
        BigDecimal inProgressValueAll = BigDecimal.ZERO;
        Map<Integer, StatusMonthlyAggregate> monthlyStatus = new HashMap<>();

        OrganizationAggregate(String organizationName, String organizationCategory,
                            Long ownerId, String ownerName, String ownerEmail) {
            this.organizationName = organizationName;
            this.organizationCategory = organizationCategory;
            this.ownerId = ownerId;
            this.ownerName = ownerName;
            this.ownerEmail = ownerEmail;
        }
    }
}
