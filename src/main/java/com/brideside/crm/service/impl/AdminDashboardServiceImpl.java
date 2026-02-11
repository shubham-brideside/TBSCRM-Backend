package com.brideside.crm.service.impl;

import com.brideside.crm.dto.AdminDashboardDtos;
import com.brideside.crm.entity.Activity;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.DealStatus;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.User;
import com.brideside.crm.repository.ActivityRepository;
import com.brideside.crm.repository.DealRepository;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.AdminDashboardService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class AdminDashboardServiceImpl implements AdminDashboardService {

    private final DealRepository dealRepository;
    private final ActivityRepository activityRepository;
    private final UserRepository userRepository;

    public AdminDashboardServiceImpl(
            DealRepository dealRepository,
            ActivityRepository activityRepository,
            UserRepository userRepository) {
        this.dealRepository = dealRepository;
        this.activityRepository = activityRepository;
        this.userRepository = userRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.WonDealsBySalesUserResponse getWonDealsBySalesUser() {
        // Reuse existing repository method that already excludes soft-deleted deals.
        List<Deal> wonDeals = dealRepository.findByStatusAndIsDeletedFalse(DealStatus.WON);

        Map<Long, AdminDashboardDtos.SalesUserWonDealsRow> aggregateByUserId = new HashMap<>();

        for (Deal deal : wonDeals) {
            User owner = resolveSalesOwnerFromPipelineOrganization(deal);
            if (owner == null || owner.getId() == null) {
                continue;
            }

            Long userId = owner.getId();
            AdminDashboardDtos.SalesUserWonDealsRow row =
                    aggregateByUserId.computeIfAbsent(userId, id -> {
                        AdminDashboardDtos.SalesUserWonDealsRow r = new AdminDashboardDtos.SalesUserWonDealsRow();
                        r.userId = id;
                        String firstName = owner.getFirstName() != null ? owner.getFirstName() : "";
                        String lastName = owner.getLastName() != null ? owner.getLastName() : "";
                        r.userName = (firstName + " " + lastName).trim();
                        r.email = owner.getEmail();
                        r.totalDeals = 0L;
                        r.totalDealValue = BigDecimal.ZERO;
                        return r;
                    });

            row.totalDeals = row.totalDeals + 1;
            BigDecimal dealValue = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;
            row.totalDealValue = row.totalDealValue.add(dealValue);
        }

        AdminDashboardDtos.WonDealsBySalesUserResponse response =
                new AdminDashboardDtos.WonDealsBySalesUserResponse();
        response.users = new ArrayList<>(aggregateByUserId.values());
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.LostDealsBySalesUserResponse getLostDealsBySalesUser() {
        // Same attribution logic as WON, but for LOST deals.
        List<Deal> lostDeals = dealRepository.findByStatusAndIsDeletedFalse(DealStatus.LOST);

        Map<Long, AdminDashboardDtos.SalesUserLostDealsRow> aggregateByUserId = new HashMap<>();

        for (Deal deal : lostDeals) {
            User owner = resolveSalesOwnerFromPipelineOrganization(deal);
            if (owner == null || owner.getId() == null) {
                continue;
            }

            Long userId = owner.getId();
            AdminDashboardDtos.SalesUserLostDealsRow row =
                    aggregateByUserId.computeIfAbsent(userId, id -> {
                        AdminDashboardDtos.SalesUserLostDealsRow r = new AdminDashboardDtos.SalesUserLostDealsRow();
                        r.userId = id;
                        String firstName = owner.getFirstName() != null ? owner.getFirstName() : "";
                        String lastName = owner.getLastName() != null ? owner.getLastName() : "";
                        r.userName = (firstName + " " + lastName).trim();
                        r.email = owner.getEmail();
                        r.totalDeals = 0L;
                        r.totalDealValue = BigDecimal.ZERO;
                        return r;
                    });

            row.totalDeals = row.totalDeals + 1;
            BigDecimal dealValue = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;
            row.totalDealValue = row.totalDealValue.add(dealValue);
        }

        AdminDashboardDtos.LostDealsBySalesUserResponse response =
                new AdminDashboardDtos.LostDealsBySalesUserResponse();
        response.users = new ArrayList<>(aggregateByUserId.values());
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.MonthlyWonDealsBySalesUserResponse getWonDealsBySalesUserMonthly(
            Integer year,
            String category,
            LocalDate dateFrom,
            LocalDate dateTo
    ) {
        if (year == null) {
            throw new IllegalArgumentException("year is required");
        }

        Organization.OrganizationCategory categoryFilter = null;
        if (category != null && !category.trim().isEmpty()) {
            categoryFilter = Organization.OrganizationCategory.fromDbValue(category);
        }

        List<Deal> wonDeals = dealRepository.findByStatusAndIsDeletedFalse(DealStatus.WON);

        // userId -> (month -> aggregate)
        Map<Long, Map<Integer, MonthlyAggregate>> aggregates = new HashMap<>();

        for (Deal deal : wonDeals) {
            LocalDateTime reference =
                    deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt();
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

            User owner = resolveSalesOwnerFromPipelineOrganization(deal);
            if (owner == null || owner.getId() == null) {
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

            Map<Integer, MonthlyAggregate> byMonth =
                    aggregates.computeIfAbsent(userId, id -> new HashMap<>());
            MonthlyAggregate agg =
                    byMonth.computeIfAbsent(month, m -> new MonthlyAggregate(owner));

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

            AdminDashboardDtos.SalesUserMonthlyWonDealsRow row =
                    new AdminDashboardDtos.SalesUserMonthlyWonDealsRow();
            row.userId = userId;

            // Use any aggregate instance to populate static user fields
            MonthlyAggregate sample = byMonth.values().iterator().next();
            row.userName = sample.userName;
            row.email = sample.email;

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
    public AdminDashboardDtos.MonthlyLostDealsBySalesUserResponse getLostDealsBySalesUserMonthly(
            Integer year,
            String category,
            LocalDate dateFrom,
            LocalDate dateTo
    ) {
        if (year == null) {
            throw new IllegalArgumentException("year is required");
        }

        Organization.OrganizationCategory categoryFilter = null;
        if (category != null && !category.trim().isEmpty()) {
            categoryFilter = Organization.OrganizationCategory.fromDbValue(category);
        }

        List<Deal> lostDeals = dealRepository.findByStatusAndIsDeletedFalse(DealStatus.LOST);

        // userId -> (month -> aggregate)
        Map<Long, Map<Integer, MonthlyAggregate>> aggregates = new HashMap<>();

        for (Deal deal : lostDeals) {
            LocalDateTime reference =
                    deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt();
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

            User owner = resolveSalesOwnerFromPipelineOrganization(deal);
            if (owner == null || owner.getId() == null) {
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

            Map<Integer, MonthlyAggregate> byMonth =
                    aggregates.computeIfAbsent(userId, id -> new HashMap<>());
            MonthlyAggregate agg =
                    byMonth.computeIfAbsent(month, m -> new MonthlyAggregate(owner));

            agg.totalDeals = agg.totalDeals + 1;
            BigDecimal dealValue = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;
            agg.totalDealValue = agg.totalDealValue.add(dealValue);
        }

        List<AdminDashboardDtos.SalesUserMonthlyLostDealsRow> userRows = new ArrayList<>();
        for (Map.Entry<Long, Map<Integer, MonthlyAggregate>> entry : aggregates.entrySet()) {
            Long userId = entry.getKey();
            Map<Integer, MonthlyAggregate> byMonth = entry.getValue();

            AdminDashboardDtos.SalesUserMonthlyLostDealsRow row =
                    new AdminDashboardDtos.SalesUserMonthlyLostDealsRow();
            row.userId = userId;

            // Use any aggregate instance to populate static user fields
            MonthlyAggregate sample = byMonth.values().iterator().next();
            row.userName = sample.userName;
            row.email = sample.email;

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

        AdminDashboardDtos.MonthlyLostDealsBySalesUserResponse response =
                new AdminDashboardDtos.MonthlyLostDealsBySalesUserResponse();
        response.year = year;
        response.category = categoryFilter != null ? categoryFilter.getDbValue() : null;
        response.users = userRows;
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.DealStatusMonthlySummaryResponse getDealStatusMonthlySummary(Integer year) {
        if (year == null) {
            throw new IllegalArgumentException("year is required");
        }

        // Load all non-deleted deals once and then bucket by status/month.
        java.util.List<Deal> deals = dealRepository.findByIsDeletedFalse();

        // month -> aggregate
        java.util.Map<Integer, StatusMonthlyAggregate> aggregates = new java.util.HashMap<>();

        for (Deal deal : deals) {
            if (deal.getStatus() == null) {
                continue;
            }

            LocalDateTime reference =
                    deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt();
            if (reference == null || reference.getYear() != year) {
                continue;
            }

            int month = reference.getMonthValue(); // 1-12
            StatusMonthlyAggregate agg =
                    aggregates.computeIfAbsent(month, m -> new StatusMonthlyAggregate());

            BigDecimal value = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;

            // Resolve organization category (if any) for per-category breakdown.
            String categoryKey = null;
            Organization org = deal.getOrganization();
            if (org != null && org.getCategory() != null) {
                categoryKey = org.getCategory().getDbValue();
            }
            StatusByCategory catAgg = null;
            if (categoryKey != null) {
                catAgg = agg.byCategory.computeIfAbsent(categoryKey, k -> new StatusByCategory());
            }

            // Resolve SALES owner for per-user breakdown.
            User owner = resolveSalesOwnerFromPipelineOrganization(deal);
            StatusByUser userAgg = null;
            if (owner != null && owner.getId() != null) {
                Long userId = owner.getId();
                userAgg = agg.byUser.computeIfAbsent(userId, id -> new StatusByUser(owner));
            }

            // Resolve pipeline for per-pipeline breakdown.
            StatusByPipeline pipelineAgg = null;
            if (deal.getPipeline() != null && deal.getPipeline().getId() != null) {
                Long pipelineId = deal.getPipeline().getId();
                pipelineAgg = agg.byPipeline.computeIfAbsent(pipelineId, id -> new StatusByPipeline(deal));
            }

            if (deal.getStatus() == DealStatus.WON) {
                agg.wonCount = agg.wonCount + 1;
                agg.wonValue = agg.wonValue.add(value);
                if (catAgg != null) {
                    catAgg.wonCount = catAgg.wonCount + 1;
                    catAgg.wonValue = catAgg.wonValue.add(value);
                }
                if (userAgg != null) {
                    userAgg.wonCount = userAgg.wonCount + 1;
                    userAgg.wonValue = userAgg.wonValue.add(value);
                }
                if (pipelineAgg != null) {
                    pipelineAgg.wonCount = pipelineAgg.wonCount + 1;
                    pipelineAgg.wonValue = pipelineAgg.wonValue.add(value);
                }
            } else if (deal.getStatus() == DealStatus.LOST) {
                agg.lostCount = agg.lostCount + 1;
                agg.lostValue = agg.lostValue.add(value);
                if (catAgg != null) {
                    catAgg.lostCount = catAgg.lostCount + 1;
                    catAgg.lostValue = catAgg.lostValue.add(value);
                }
                if (userAgg != null) {
                    userAgg.lostCount = userAgg.lostCount + 1;
                    userAgg.lostValue = userAgg.lostValue.add(value);
                }
                if (pipelineAgg != null) {
                    pipelineAgg.lostCount = pipelineAgg.lostCount + 1;
                    pipelineAgg.lostValue = pipelineAgg.lostValue.add(value);
                }
            } else if (deal.getStatus() == DealStatus.IN_PROGRESS) {
                agg.inProgressCount = agg.inProgressCount + 1;
                agg.inProgressValue = agg.inProgressValue.add(value);
                if (catAgg != null) {
                    catAgg.inProgressCount = catAgg.inProgressCount + 1;
                    catAgg.inProgressValue = catAgg.inProgressValue.add(value);
                }
                if (userAgg != null) {
                    userAgg.inProgressCount = userAgg.inProgressCount + 1;
                    userAgg.inProgressValue = userAgg.inProgressValue.add(value);
                }
                if (pipelineAgg != null) {
                    pipelineAgg.inProgressCount = pipelineAgg.inProgressCount + 1;
                    pipelineAgg.inProgressValue = pipelineAgg.inProgressValue.add(value);
                }
            }
        }

        java.util.List<AdminDashboardDtos.DealStatusMonthlyRow> monthRows = new java.util.ArrayList<>();
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

                // Build per-category breakdown for this month
                java.util.List<AdminDashboardDtos.DealStatusCategoryRow> categoryRows =
                        new java.util.ArrayList<>();
                for (java.util.Map.Entry<String, StatusByCategory> entry
                        : agg.byCategory.entrySet()) {
                    AdminDashboardDtos.DealStatusCategoryRow catRow =
                            new AdminDashboardDtos.DealStatusCategoryRow();
                    catRow.category = entry.getKey();
                    StatusByCategory s = entry.getValue();
                    catRow.wonCount = s.wonCount;
                    catRow.wonValue = s.wonValue;
                    catRow.lostCount = s.lostCount;
                    catRow.lostValue = s.lostValue;
                    catRow.inProgressCount = s.inProgressCount;
                    catRow.inProgressValue = s.inProgressValue;
                    categoryRows.add(catRow);
                }
                row.categories = categoryRows;

                // Build per-sales-user breakdown for this month
                java.util.List<AdminDashboardDtos.DealStatusMonthlyUserRow> userRows =
                        new java.util.ArrayList<>();
                for (java.util.Map.Entry<Long, StatusByUser> entry
                        : agg.byUser.entrySet()) {
                    AdminDashboardDtos.DealStatusMonthlyUserRow userRow =
                            new AdminDashboardDtos.DealStatusMonthlyUserRow();
                    StatusByUser s = entry.getValue();
                    userRow.userId = s.userId;
                    userRow.userName = s.userName;
                    userRow.email = s.email;
                    userRow.wonCount = s.wonCount;
                    userRow.wonValue = s.wonValue;
                    userRow.lostCount = s.lostCount;
                    userRow.lostValue = s.lostValue;
                    userRow.inProgressCount = s.inProgressCount;
                    userRow.inProgressValue = s.inProgressValue;
                    userRows.add(userRow);
                }
                row.users = userRows;

                // Build per-pipeline breakdown for this month
                java.util.List<AdminDashboardDtos.DealStatusPipelineRow> pipelineRows =
                        new java.util.ArrayList<>();
                for (java.util.Map.Entry<Long, StatusByPipeline> entry
                        : agg.byPipeline.entrySet()) {
                    StatusByPipeline s = entry.getValue();
                    AdminDashboardDtos.DealStatusPipelineRow pRow =
                            new AdminDashboardDtos.DealStatusPipelineRow();
                    pRow.pipelineId = s.pipelineId;
                    pRow.pipelineName = s.pipelineName;
                    pRow.wonCount = s.wonCount;
                    pRow.wonValue = s.wonValue;
                    pRow.lostCount = s.lostCount;
                    pRow.lostValue = s.lostValue;
                    pRow.inProgressCount = s.inProgressCount;
                    pRow.inProgressValue = s.inProgressValue;
                    pipelineRows.add(pRow);
                }
                row.pipelines = pipelineRows;
            } else {
                row.wonCount = 0L;
                row.wonValue = BigDecimal.ZERO;
                row.lostCount = 0L;
                row.lostValue = BigDecimal.ZERO;
                row.inProgressCount = 0L;
                row.inProgressValue = BigDecimal.ZERO;
                row.categories = new java.util.ArrayList<>();
                row.users = new java.util.ArrayList<>();
                row.pipelines = new java.util.ArrayList<>();
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
    public AdminDashboardDtos.UserActivityMonthlySummaryResponse getUserActivityMonthlySummary(Integer year) {
        if (year == null) {
            throw new IllegalArgumentException("year is required");
        }

        List<Activity> activities = activityRepository.findAll();

        // userId -> (month -> aggregate)
        Map<Long, Map<Integer, ActivityMonthlyAggregate>> aggregates = new HashMap<>();

        for (Activity activity : activities) {
            Long userId = activity.getAssignedUserId();
            if (userId == null) {
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
                            .collect(java.util.stream.Collectors.toMap(
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

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.OrganizationDealStatusSummaryResponse getOrganizationDealStatusSummary(Integer year) {
        if (year == null) {
            throw new IllegalArgumentException("year is required");
        }

        // Load all non-deleted deals and bucket them per organization.
        List<Deal> deals = dealRepository.findByIsDeletedFalse();

        // orgId -> aggregate
        Map<Long, OrganizationAggregate> aggregates = new HashMap<>();

        for (Deal deal : deals) {
            if (deal.getOrganization() == null || deal.getOrganization().getId() == null) {
                continue;
            }
            Organization org = deal.getOrganization();
            Long orgId = org.getId();
            String orgName = org.getName();

            OrganizationAggregate agg =
                    aggregates.computeIfAbsent(orgId, id -> {
                        String category = null;
                        if (org.getCategory() != null) {
                            category = org.getCategory().getDbValue();
                        }

                        Long ownerId = null;
                        String ownerName = null;
                        String ownerEmail = null;
                        User owner = org.getOwner();
                        if (owner != null && owner.getId() != null) {
                            ownerId = owner.getId();
                            String firstName = owner.getFirstName() != null ? owner.getFirstName() : "";
                            String lastName = owner.getLastName() != null ? owner.getLastName() : "";
                            ownerName = (firstName + " " + lastName).trim();
                            ownerEmail = owner.getEmail();
                        }

                        return new OrganizationAggregate(orgName, category, ownerId, ownerName, ownerEmail);
                    });

            BigDecimal value = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;

            // ---- All-time totals ----
            if (deal.getStatus() == DealStatus.WON) {
                agg.wonCountAll = agg.wonCountAll + 1;
                agg.wonValueAll = agg.wonValueAll.add(value);
            } else if (deal.getStatus() == DealStatus.LOST) {
                agg.lostCountAll = agg.lostCountAll + 1;
                agg.lostValueAll = agg.lostValueAll.add(value);
            } else if (deal.getStatus() == DealStatus.IN_PROGRESS) {
                agg.inProgressCountAll = agg.inProgressCountAll + 1;
                agg.inProgressValueAll = agg.inProgressValueAll.add(value);
            }

            // ---- Yearly/monthly breakdown ----
            if (deal.getStatus() != null) {
                LocalDateTime reference =
                        deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt();
                if (reference != null && reference.getYear() == year) {
                    int month = reference.getMonthValue(); // 1-12
                    StatusMonthlyAggregate statusAgg =
                            agg.monthlyStatus.computeIfAbsent(month, m -> new StatusMonthlyAggregate());

                    if (deal.getStatus() == DealStatus.WON) {
                        statusAgg.wonCount = statusAgg.wonCount + 1;
                        statusAgg.wonValue = statusAgg.wonValue.add(value);
                    } else if (deal.getStatus() == DealStatus.LOST) {
                        statusAgg.lostCount = statusAgg.lostCount + 1;
                        statusAgg.lostValue = statusAgg.lostValue.add(value);
                    } else if (deal.getStatus() == DealStatus.IN_PROGRESS) {
                        statusAgg.inProgressCount = statusAgg.inProgressCount + 1;
                        statusAgg.inProgressValue = statusAgg.inProgressValue.add(value);
                    }
                }
            }
        }

        List<AdminDashboardDtos.OrganizationDealStatusRow> orgRows = new ArrayList<>();
        for (Map.Entry<Long, OrganizationAggregate> entry : aggregates.entrySet()) {
            Long orgId = entry.getKey();
            OrganizationAggregate agg = entry.getValue();

            AdminDashboardDtos.OrganizationDealStatusRow row =
                    new AdminDashboardDtos.OrganizationDealStatusRow();
            row.organizationId = orgId;
            row.organizationName = agg.organizationName;
            row.organizationCategory = agg.organizationCategory;
            row.ownerId = agg.ownerId;
            row.ownerName = agg.ownerName;
            row.ownerEmail = agg.ownerEmail;

            // All-time totals
            row.wonCountAll = agg.wonCountAll;
            row.wonValueAll = agg.wonValueAll;
            row.lostCountAll = agg.lostCountAll;
            row.lostValueAll = agg.lostValueAll;
            row.inProgressCountAll = agg.inProgressCountAll;
            row.inProgressValueAll = agg.inProgressValueAll;

            // Monthly rows for requested year
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

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.LostReasonSummaryResponse getLostReasonSummary(
            String category,
            Long userId,
            Long pipelineId
    ) {
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

            // Optional organization category filter based on deal.organization.category
            if (categoryFilter != null) {
                Organization org = deal.getOrganization();
                if (org == null || org.getCategory() == null || org.getCategory() != categoryFilter) {
                    continue;
                }
            }

            // Optional SALES user filter (via pipeline -> organization -> owner)
            if (userId != null) {
                User owner = resolveSalesOwnerFromPipelineOrganization(deal);
                if (owner == null || owner.getId() == null || !userId.equals(owner.getId())) {
                    continue;
                }
            }

            // Optional pipeline filter
            if (pipelineId != null) {
                if (deal.getPipeline() == null || deal.getPipeline().getId() == null
                        || !pipelineId.equals(deal.getPipeline().getId())) {
                    continue;
                }
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
            if (totalLost > 0) {
                BigDecimal pct = BigDecimal.valueOf(entry.getValue() * 100.0)
                        .divide(BigDecimal.valueOf(totalLost), 2, java.math.RoundingMode.HALF_UP);
                row.percentage = pct;
            } else {
                row.percentage = BigDecimal.ZERO;
            }
            rows.add(row);
        }

        AdminDashboardDtos.LostReasonSummaryResponse response =
                new AdminDashboardDtos.LostReasonSummaryResponse();
        response.totalLostDeals = totalLost;
        response.category = categoryFilter != null ? categoryFilter.getDbValue() : null;
        response.userId = userId;
        response.pipelineId = pipelineId;
        response.reasons = rows;
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public AdminDashboardDtos.RevenueSummaryResponse getRevenueSummary(
            LocalDate dateFrom,
            LocalDate dateTo
    ) {
        if (dateFrom == null || dateTo == null) {
            throw new IllegalArgumentException("dateFrom and dateTo are required");
        }
        if (dateTo.isBefore(dateFrom)) {
            throw new IllegalArgumentException("dateTo must be on or after dateFrom");
        }

        LocalDateTime fromDateTime = dateFrom.atStartOfDay();
        LocalDateTime toDateTime = dateTo.plusDays(1).atStartOfDay(); // inclusive end

        // Start from all WON, non-deleted deals, then filter by date range.
        List<Deal> wonDeals = dealRepository.findByStatusAndIsDeletedFalse(DealStatus.WON);

        long totalDeals = 0L;
        BigDecimal totalValue = BigDecimal.ZERO;

        Map<String, CategoryRevenueAggregate> byCategory = new HashMap<>();
        Map<Long, UserRevenueAggregate> byUser = new HashMap<>();
        Map<Long, PipelineRevenueAggregate> byPipeline = new HashMap<>();

        for (Deal deal : wonDeals) {
            LocalDateTime reference =
                    deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt();
            if (reference == null) {
                continue;
            }
            if (reference.isBefore(fromDateTime) || !reference.isBefore(toDateTime)) {
                continue;
            }

            BigDecimal value = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;

            totalDeals++;
            totalValue = totalValue.add(value);

            // ---- By organization category ----
            String categoryKey = null;
            if (deal.getOrganization() != null && deal.getOrganization().getCategory() != null) {
                categoryKey = deal.getOrganization().getCategory().getDbValue();
            }
            if (categoryKey != null) {
                CategoryRevenueAggregate catAgg =
                        byCategory.computeIfAbsent(categoryKey, k -> new CategoryRevenueAggregate());
                catAgg.totalDeals = catAgg.totalDeals + 1;
                catAgg.totalValue = catAgg.totalValue.add(value);
            }

            // ---- By SALES user (via pipeline -> organization -> owner) ----
            User owner = resolveSalesOwnerFromPipelineOrganization(deal);
            if (owner != null && owner.getId() != null) {
                Long userId = owner.getId();
                UserRevenueAggregate userAgg =
                        byUser.computeIfAbsent(userId, id -> new UserRevenueAggregate(owner));
                userAgg.totalDeals = userAgg.totalDeals + 1;
                userAgg.totalValue = userAgg.totalValue.add(value);
            }

            // ---- By pipeline ----
            if (deal.getPipeline() != null && deal.getPipeline().getId() != null) {
                Long pipelineId = deal.getPipeline().getId();
                PipelineRevenueAggregate pipeAgg =
                        byPipeline.computeIfAbsent(pipelineId, id -> new PipelineRevenueAggregate(deal));
                pipeAgg.totalDeals = pipeAgg.totalDeals + 1;
                pipeAgg.totalValue = pipeAgg.totalValue.add(value);
            }
        }

        AdminDashboardDtos.RevenueSummaryResponse response =
                new AdminDashboardDtos.RevenueSummaryResponse();
        response.dateFrom = dateFrom;
        response.dateTo = dateTo;
        response.totalDeals = totalDeals;
        response.totalDealValue = totalValue;

        List<AdminDashboardDtos.RevenueByCategoryRow> categoryRows = new ArrayList<>();
        for (Map.Entry<String, CategoryRevenueAggregate> entry : byCategory.entrySet()) {
            AdminDashboardDtos.RevenueByCategoryRow row =
                    new AdminDashboardDtos.RevenueByCategoryRow();
            row.category = entry.getKey();
            row.totalDeals = entry.getValue().totalDeals;
            row.totalDealValue = entry.getValue().totalValue;
            categoryRows.add(row);
        }
        response.categories = categoryRows;

        List<AdminDashboardDtos.RevenueByUserRow> userRows = new ArrayList<>();
        for (UserRevenueAggregate agg : byUser.values()) {
            AdminDashboardDtos.RevenueByUserRow row =
                    new AdminDashboardDtos.RevenueByUserRow();
            row.userId = agg.userId;
            row.userName = agg.userName;
            row.email = agg.email;
            row.totalDeals = agg.totalDeals;
            row.totalDealValue = agg.totalValue;
            userRows.add(row);
        }
        response.users = userRows;

        List<AdminDashboardDtos.RevenueByPipelineRow> pipelineRows = new ArrayList<>();
        for (PipelineRevenueAggregate agg : byPipeline.values()) {
            AdminDashboardDtos.RevenueByPipelineRow row =
                    new AdminDashboardDtos.RevenueByPipelineRow();
            row.pipelineId = agg.pipelineId;
            row.pipelineName = agg.pipelineName;
            row.totalDeals = agg.totalDeals;
            row.totalDealValue = agg.totalValue;
            pipelineRows.add(row);
        }
        response.pipelines = pipelineRows;

        return response;
    }

    private User resolveSalesOwnerFromPipelineOrganization(Deal deal) {
        if (deal == null || deal.getPipeline() == null) {
            return null;
        }

        Organization organization = deal.getPipeline().getOrganization();
        if (organization == null) {
            return null;
        }

        User owner = organization.getOwner();
        if (owner == null || owner.getId() == null) {
            return null;
        }

        if (owner.getRole() == null || owner.getRole().getName() != Role.RoleName.SALES) {
            return null;
        }

        return owner;
    }

    private static class MonthlyAggregate {
        final String userName;
        final String email;
        Long totalDeals = 0L;
        BigDecimal totalDealValue = BigDecimal.ZERO;
        BigDecimal totalDealCommission = BigDecimal.ZERO;

        MonthlyAggregate(User owner) {
            String firstName = owner.getFirstName() != null ? owner.getFirstName() : "";
            String lastName = owner.getLastName() != null ? owner.getLastName() : "";
            this.userName = (firstName + " " + lastName).trim();
            this.email = owner.getEmail();
        }
    }

    private static class StatusMonthlyAggregate {
        Long wonCount = 0L;
        BigDecimal wonValue = BigDecimal.ZERO;
        Long lostCount = 0L;
        BigDecimal lostValue = BigDecimal.ZERO;
        Long inProgressCount = 0L;
        BigDecimal inProgressValue = BigDecimal.ZERO;
        // organization category (dbValue) -> per-category aggregate for this month
        java.util.Map<String, StatusByCategory> byCategory = new java.util.HashMap<>();
        // sales userId -> per-user aggregate for this month
        java.util.Map<Long, StatusByUser> byUser = new java.util.HashMap<>();
        // pipelineId -> per-pipeline aggregate for this month
        java.util.Map<Long, StatusByPipeline> byPipeline = new java.util.HashMap<>();
    }

    private static class StatusByCategory {
        Long wonCount = 0L;
        BigDecimal wonValue = BigDecimal.ZERO;
        Long lostCount = 0L;
        BigDecimal lostValue = BigDecimal.ZERO;
        Long inProgressCount = 0L;
        BigDecimal inProgressValue = BigDecimal.ZERO;
    }

    private static class StatusByUser {
        final Long userId;
        final String userName;
        final String email;

        Long wonCount = 0L;
        BigDecimal wonValue = BigDecimal.ZERO;
        Long lostCount = 0L;
        BigDecimal lostValue = BigDecimal.ZERO;
        Long inProgressCount = 0L;
        BigDecimal inProgressValue = BigDecimal.ZERO;

        StatusByUser(User owner) {
            this.userId = owner.getId();
            String firstName = owner.getFirstName() != null ? owner.getFirstName() : "";
            String lastName = owner.getLastName() != null ? owner.getLastName() : "";
            this.userName = (firstName + " " + lastName).trim();
            this.email = owner.getEmail();
        }
    }

    private static class StatusByPipeline {
        final Long pipelineId;
        final String pipelineName;

        Long wonCount = 0L;
        BigDecimal wonValue = BigDecimal.ZERO;
        Long lostCount = 0L;
        BigDecimal lostValue = BigDecimal.ZERO;
        Long inProgressCount = 0L;
        BigDecimal inProgressValue = BigDecimal.ZERO;

        StatusByPipeline(Deal deal) {
            this.pipelineId = deal.getPipeline() != null ? deal.getPipeline().getId() : null;
            this.pipelineName = deal.getPipeline() != null ? deal.getPipeline().getName() : null;
        }
    }

    private static class CategoryRevenueAggregate {
        Long totalDeals = 0L;
        BigDecimal totalValue = BigDecimal.ZERO;
    }

    private static class UserRevenueAggregate {
        final Long userId;
        final String userName;
        final String email;
        Long totalDeals = 0L;
        BigDecimal totalValue = BigDecimal.ZERO;

        UserRevenueAggregate(User owner) {
            this.userId = owner.getId();
            String firstName = owner.getFirstName() != null ? owner.getFirstName() : "";
            String lastName = owner.getLastName() != null ? owner.getLastName() : "";
            this.userName = (firstName + " " + lastName).trim();
            this.email = owner.getEmail();
        }
    }

    private static class PipelineRevenueAggregate {
        final Long pipelineId;
        final String pipelineName;
        Long totalDeals = 0L;
        BigDecimal totalValue = BigDecimal.ZERO;

        PipelineRevenueAggregate(Deal deal) {
            this.pipelineId = deal.getPipeline() != null ? deal.getPipeline().getId() : null;
            this.pipelineName = deal.getPipeline() != null ? deal.getPipeline().getName() : null;
        }
    }

    private static class ActivityMonthlyAggregate {
        Long totalActivities = 0L;
        Long callCount = 0L;
        Long totalCallMinutes = 0L;
        Long meetingCount = 0L;
        Long totalMeetingMinutes = 0L;
    }

    private static class OrganizationAggregate {
        final String organizationName;
        final String organizationCategory;
        final Long ownerId;
        final String ownerName;
        final String ownerEmail;

        Long wonCountAll = 0L;
        BigDecimal wonValueAll = BigDecimal.ZERO;
        Long lostCountAll = 0L;
        BigDecimal lostValueAll = BigDecimal.ZERO;
        Long inProgressCountAll = 0L;
        BigDecimal inProgressValueAll = BigDecimal.ZERO;

        // month -> StatusMonthlyAggregate for this org
        Map<Integer, StatusMonthlyAggregate> monthlyStatus = new HashMap<>();

        OrganizationAggregate(
                String organizationName,
                String organizationCategory,
                Long ownerId,
                String ownerName,
                String ownerEmail
        ) {
            this.organizationName = organizationName;
            this.organizationCategory = organizationCategory;
            this.ownerId = ownerId;
            this.ownerName = ownerName;
            this.ownerEmail = ownerEmail;
        }
    }
}

