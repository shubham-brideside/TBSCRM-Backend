package com.brideside.crm.service.impl;

import com.brideside.crm.dto.SalesDashboardDtos;
import com.brideside.crm.entity.*;
import com.brideside.crm.repository.ActivityRepository;
import com.brideside.crm.repository.DealRepository;
import com.brideside.crm.repository.SalesTargetRepository;
import com.brideside.crm.service.SalesDashboardService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class SalesDashboardServiceImpl implements SalesDashboardService {

    private final DealRepository dealRepository;
    private final ActivityRepository activityRepository;
    private final SalesTargetRepository salesTargetRepository;

    public SalesDashboardServiceImpl(DealRepository dealRepository,
                                     ActivityRepository activityRepository,
                                     SalesTargetRepository salesTargetRepository) {
        this.dealRepository = dealRepository;
        this.activityRepository = activityRepository;
        this.salesTargetRepository = salesTargetRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public SalesDashboardDtos.SummaryResponse getDashboardSummary(User currentUser) {
        List<Deal> userDeals = getUserDeals(dealRepository.findByIsDeletedFalse(), currentUser);

        long wonCount = 0, lostCount = 0, inProgressCount = 0;
        BigDecimal wonValue = BigDecimal.ZERO, lostValue = BigDecimal.ZERO, inProgressValue = BigDecimal.ZERO;
        BigDecimal totalCommission = BigDecimal.ZERO;
        BigDecimal wonValueYtd = BigDecimal.ZERO;
        int currentYear = LocalDate.now().getYear();

        for (Deal deal : userDeals) {
            BigDecimal v = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;
            BigDecimal comm = deal.getCommissionAmount() != null ? deal.getCommissionAmount() : BigDecimal.ZERO;

            if (deal.getStatus() == DealStatus.WON) {
                wonCount++;
                wonValue = wonValue.add(v);
                totalCommission = totalCommission.add(comm);
                LocalDateTime ref = deal.getWonAt() != null ? deal.getWonAt()
                        : (deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt());
                if (ref != null && ref.getYear() == currentYear) {
                    wonValueYtd = wonValueYtd.add(v);
                }
            } else if (deal.getStatus() == DealStatus.LOST) {
                lostCount++;
                lostValue = lostValue.add(v);
            } else if (deal.getStatus() == DealStatus.IN_PROGRESS) {
                inProgressCount++;
                inProgressValue = inProgressValue.add(v);
            }
        }

        List<Activity> activities = activityRepository.findByAssignedUserId(currentUser.getId());
        long totalActivities = activities.size();
        long pendingActivities = activities.stream()
                .filter(a -> a.getStatus() != null
                        && (a.getStatus() == Activity.ActivityStatus.OPEN
                        || a.getStatus() == Activity.ActivityStatus.PENDING
                        || a.getStatus() == Activity.ActivityStatus.IN_PROGRESS))
                .count();

        SalesDashboardDtos.SummaryResponse r = new SalesDashboardDtos.SummaryResponse();
        r.userId = currentUser.getId();
        r.userName = fullName(currentUser);
        r.email = currentUser.getEmail();
        r.totalDeals = (long) userDeals.size();
        r.wonDeals = wonCount;
        r.lostDeals = lostCount;
        r.inProgressDeals = inProgressCount;
        r.totalDealValue = wonValue.add(lostValue).add(inProgressValue);
        r.wonDealValue = wonValue;
        r.lostDealValue = lostValue;
        r.inProgressDealValue = inProgressValue;
        r.totalCommission = totalCommission;
        r.wonValueYtd = wonValueYtd;
        r.totalActivities = totalActivities;
        r.pendingActivities = pendingActivities;
        return r;
    }

    @Override
    @Transactional(readOnly = true)
    public SalesDashboardDtos.DealStatusMonthlyResponse getDealStatusMonthly(User currentUser, Integer year) {
        List<Deal> userDeals = getUserDeals(dealRepository.findByIsDeletedFalse(), currentUser);
        Map<Integer, StatusAggregate> byMonth = new HashMap<>();

        for (Deal deal : userDeals) {
            if (deal.getStatus() == null) continue;
            LocalDateTime ref = getReferenceDateForStatus(deal);
            if (ref == null || ref.getYear() != year) continue;

            int month = ref.getMonthValue();
            StatusAggregate agg = byMonth.computeIfAbsent(month, m -> new StatusAggregate());
            BigDecimal v = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;
            agg.add(deal.getStatus(), v);
        }

        List<SalesDashboardDtos.DealStatusMonthRow> rows = new ArrayList<>();
        for (int m = 1; m <= 12; m++) {
            StatusAggregate agg = byMonth.get(m);
            SalesDashboardDtos.DealStatusMonthRow row = new SalesDashboardDtos.DealStatusMonthRow();
            row.month = m;
            if (agg != null) {
                row.wonCount = agg.wonCount;
                row.wonValue = agg.wonValue;
                row.lostCount = agg.lostCount;
                row.lostValue = agg.lostValue;
                row.inProgressCount = agg.inProgressCount;
                row.inProgressValue = agg.inProgressValue;
            } else {
                row.wonCount = 0L;
                row.wonValue = BigDecimal.ZERO;
                row.lostCount = 0L;
                row.lostValue = BigDecimal.ZERO;
                row.inProgressCount = 0L;
                row.inProgressValue = BigDecimal.ZERO;
            }
            rows.add(row);
        }

        SalesDashboardDtos.DealStatusMonthlyResponse response = new SalesDashboardDtos.DealStatusMonthlyResponse();
        response.year = year;
        response.months = rows;
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public SalesDashboardDtos.RevenueResponse getRevenue(User currentUser, LocalDate dateFrom, LocalDate dateTo) {
        List<Deal> wonDeals = getUserDeals(
                dealRepository.findByStatusAndIsDeletedFalse(DealStatus.WON), currentUser);

        LocalDateTime fromDt = dateFrom.atStartOfDay();
        LocalDateTime toDt = dateTo.plusDays(1).atStartOfDay();

        long totalDeals = 0;
        BigDecimal totalValue = BigDecimal.ZERO;
        BigDecimal totalCommission = BigDecimal.ZERO;
        Map<Long, PipelineRevAggregate> byPipeline = new LinkedHashMap<>();

        for (Deal deal : wonDeals) {
            LocalDateTime ref = deal.getWonAt() != null ? deal.getWonAt()
                    : (deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt());
            if (ref == null || ref.isBefore(fromDt) || !ref.isBefore(toDt)) continue;

            BigDecimal v = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;
            BigDecimal c = deal.getCommissionAmount() != null ? deal.getCommissionAmount() : BigDecimal.ZERO;
            totalDeals++;
            totalValue = totalValue.add(v);
            totalCommission = totalCommission.add(c);

            if (deal.getPipeline() != null && deal.getPipeline().getId() != null) {
                Long pid = deal.getPipeline().getId();
                byPipeline.computeIfAbsent(pid, id -> new PipelineRevAggregate(deal.getPipeline()))
                        .add(v, c);
            }
        }

        SalesDashboardDtos.RevenueResponse response = new SalesDashboardDtos.RevenueResponse();
        response.dateFrom = dateFrom;
        response.dateTo = dateTo;
        response.totalDeals = totalDeals;
        response.totalDealValue = totalValue;
        response.totalCommission = totalCommission;
        response.pipelines = byPipeline.values().stream().map(agg -> {
            SalesDashboardDtos.RevenueByPipelineRow row = new SalesDashboardDtos.RevenueByPipelineRow();
            row.pipelineId = agg.pipelineId;
            row.pipelineName = agg.pipelineName;
            row.totalDeals = agg.totalDeals;
            row.totalDealValue = agg.totalValue;
            row.totalCommission = agg.totalCommission;
            return row;
        }).collect(Collectors.toList());
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public SalesDashboardDtos.LostReasonsResponse getLostReasons(User currentUser) {
        List<Deal> lostDeals = getUserDeals(
                dealRepository.findByStatusAndIsDeletedFalse(DealStatus.LOST), currentUser);

        Map<String, Long> countsByReason = new LinkedHashMap<>();
        long total = 0;

        for (Deal deal : lostDeals) {
            if (deal.getLostReason() == null) continue;
            String label = deal.getLostReason().toDisplayString();
            countsByReason.merge(label, 1L, Long::sum);
            total++;
        }

        final long finalTotal = total;
        List<SalesDashboardDtos.LostReasonRow> rows = countsByReason.entrySet().stream().map(e -> {
            SalesDashboardDtos.LostReasonRow row = new SalesDashboardDtos.LostReasonRow();
            row.reason = e.getKey();
            row.count = e.getValue();
            row.percentage = finalTotal > 0
                    ? BigDecimal.valueOf(e.getValue() * 100.0)
                    .divide(BigDecimal.valueOf(finalTotal), 2, RoundingMode.HALF_UP)
                    : BigDecimal.ZERO;
            return row;
        }).collect(Collectors.toList());

        SalesDashboardDtos.LostReasonsResponse response = new SalesDashboardDtos.LostReasonsResponse();
        response.totalLostDeals = total;
        response.reasons = rows;
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public SalesDashboardDtos.ActivityMonthlyResponse getActivityMonthly(User currentUser, Integer year) {
        List<Activity> activities = activityRepository.findByAssignedUserId(currentUser.getId());
        Map<Integer, ActivityAggregate> byMonth = new HashMap<>();

        for (Activity activity : activities) {
            if (activity.getCreatedAt() == null) continue;
            LocalDateTime ref = LocalDateTime.ofInstant(activity.getCreatedAt(), ZoneId.systemDefault());
            if (ref.getYear() != year) continue;

            int month = ref.getMonthValue();
            ActivityAggregate agg = byMonth.computeIfAbsent(month, m -> new ActivityAggregate());
            agg.totalActivities++;

            if (activity.getCategory() == Activity.ActivityCategory.CALL) {
                agg.callCount++;
                agg.totalCallMinutes += activity.getDurationMinutes() != null ? activity.getDurationMinutes() : 0;
            }
            if (activity.getCategory() == Activity.ActivityCategory.MEETING
                    || activity.getCategory() == Activity.ActivityCategory.MEETING_SCHEDULER) {
                agg.meetingCount++;
                agg.totalMeetingMinutes += activity.getDurationMinutes() != null ? activity.getDurationMinutes() : 0;
            }
        }

        List<SalesDashboardDtos.ActivityMonthRow> rows = new ArrayList<>();
        for (int m = 1; m <= 12; m++) {
            ActivityAggregate agg = byMonth.get(m);
            SalesDashboardDtos.ActivityMonthRow row = new SalesDashboardDtos.ActivityMonthRow();
            row.month = m;
            if (agg != null) {
                row.totalActivities = agg.totalActivities;
                row.callCount = agg.callCount;
                row.totalCallMinutes = agg.totalCallMinutes;
                row.meetingCount = agg.meetingCount;
                row.totalMeetingMinutes = agg.totalMeetingMinutes;
            } else {
                row.totalActivities = 0L;
                row.callCount = 0L;
                row.totalCallMinutes = 0L;
                row.meetingCount = 0L;
                row.totalMeetingMinutes = 0L;
            }
            rows.add(row);
        }

        SalesDashboardDtos.ActivityMonthlyResponse response = new SalesDashboardDtos.ActivityMonthlyResponse();
        response.year = year;
        response.months = rows;
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public SalesDashboardDtos.PipelinePerformanceResponse getPipelinePerformance(User currentUser) {
        List<Deal> userDeals = getUserDeals(dealRepository.findByIsDeletedFalse(), currentUser);
        Map<Long, PipelineStatusAggregate> byPipeline = new LinkedHashMap<>();

        for (Deal deal : userDeals) {
            if (deal.getPipeline() == null || deal.getPipeline().getId() == null) continue;
            Long pid = deal.getPipeline().getId();
            PipelineStatusAggregate agg = byPipeline.computeIfAbsent(pid,
                    id -> new PipelineStatusAggregate(deal.getPipeline()));
            BigDecimal v = deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO;
            agg.add(deal.getStatus(), v);
        }

        SalesDashboardDtos.PipelinePerformanceResponse response = new SalesDashboardDtos.PipelinePerformanceResponse();
        response.pipelines = byPipeline.values().stream().map(agg -> {
            SalesDashboardDtos.PipelinePerformanceRow row = new SalesDashboardDtos.PipelinePerformanceRow();
            row.pipelineId = agg.pipelineId;
            row.pipelineName = agg.pipelineName;
            row.wonCount = agg.wonCount;
            row.wonValue = agg.wonValue;
            row.lostCount = agg.lostCount;
            row.lostValue = agg.lostValue;
            row.inProgressCount = agg.inProgressCount;
            row.inProgressValue = agg.inProgressValue;
            return row;
        }).collect(Collectors.toList());
        return response;
    }

    @Override
    @Transactional(readOnly = true)
    public SalesDashboardDtos.TargetVsAchievementResponse getTargetVsAchievement(User currentUser, Integer year) {
        LocalDate yearStart = LocalDate.of(year, 1, 1);
        LocalDate yearEnd = LocalDate.of(year, 12, 31);
        List<SalesTarget> targets = salesTargetRepository.findByUser_IdAndPeriodStartBetween(
                currentUser.getId(), yearStart, yearEnd);

        List<Deal> wonDeals = getUserDeals(
                dealRepository.findByStatusAndIsDeletedFalse(DealStatus.WON), currentUser);

        List<SalesDashboardDtos.TargetVsAchievementRow> rows = new ArrayList<>();
        for (SalesTarget target : targets) {
            LocalDate periodStart = target.getPeriodStart();
            LocalDate periodEnd = computePeriodEnd(periodStart, target.getPeriodType());

            LocalDateTime fromDt = periodStart.atStartOfDay();
            LocalDateTime toDt = periodEnd.plusDays(1).atStartOfDay();

            Set<Long> targetOrgIds = target.getOrganizations() != null
                    ? target.getOrganizations().stream().map(Organization::getId).collect(Collectors.toSet())
                    : Collections.emptySet();

            BigDecimal achieved = BigDecimal.ZERO;
            for (Deal deal : wonDeals) {
                LocalDateTime ref = deal.getWonAt() != null ? deal.getWonAt()
                        : (deal.getUpdatedAt() != null ? deal.getUpdatedAt() : deal.getCreatedAt());
                if (ref == null || ref.isBefore(fromDt) || !ref.isBefore(toDt)) continue;

                if (!targetOrgIds.isEmpty() && deal.getOrganization() != null) {
                    if (!targetOrgIds.contains(deal.getOrganization().getId())) continue;
                }

                TargetCategory dealCat = TargetCategory.fromDeal(deal);
                if (dealCat != null && dealCat != target.getCategory()) continue;

                achieved = achieved.add(deal.getValue() != null ? deal.getValue() : BigDecimal.ZERO);
            }

            SalesDashboardDtos.TargetVsAchievementRow row = new SalesDashboardDtos.TargetVsAchievementRow();
            row.category = target.getCategory() != null ? target.getCategory().getLabel() : null;
            row.periodType = target.getPeriodType() != null ? target.getPeriodType().name() : null;
            row.periodStart = periodStart;
            row.targetAmount = target.getTargetAmount();
            row.achievedAmount = achieved;
            row.achievementPercentage = target.getTargetAmount().compareTo(BigDecimal.ZERO) > 0
                    ? achieved.multiply(BigDecimal.valueOf(100))
                    .divide(target.getTargetAmount(), 2, RoundingMode.HALF_UP)
                    : BigDecimal.ZERO;
            row.organizationNames = target.getOrganizations() != null
                    ? target.getOrganizations().stream().map(Organization::getName).collect(Collectors.toList())
                    : Collections.emptyList();
            rows.add(row);
        }

        SalesDashboardDtos.TargetVsAchievementResponse response = new SalesDashboardDtos.TargetVsAchievementResponse();
        response.year = year;
        response.targets = rows;
        return response;
    }

    // ---- Helpers ----

    /**
     * Filter deals to only those owned by the given user
     * (deal -> pipeline -> organization -> owner == user).
     */
    private List<Deal> getUserDeals(List<Deal> allDeals, User user) {
        Long userId = user.getId();
        List<Deal> result = new ArrayList<>();
        for (Deal deal : allDeals) {
            User owner = resolveOwnerFromDeal(deal);
            if (owner != null && userId.equals(owner.getId())) {
                result.add(deal);
            }
        }
        return result;
    }

    private User resolveOwnerFromDeal(Deal deal) {
        if (deal == null || deal.getPipeline() == null) return null;
        Organization org = deal.getPipeline().getOrganization();
        if (org == null) return null;
        return org.getOwner();
    }

    private static String fullName(User u) {
        String first = u.getFirstName() != null ? u.getFirstName() : "";
        String last = u.getLastName() != null ? u.getLastName() : "";
        return (first + " " + last).trim();
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

    private static LocalDate computePeriodEnd(LocalDate periodStart, SalesTarget.PeriodType type) {
        if (type == null || type == SalesTarget.PeriodType.MONTHLY) {
            return periodStart.plusMonths(1).minusDays(1);
        }
        return switch (type) {
            case QUARTERLY -> periodStart.plusMonths(3).minusDays(1);
            case HALF_YEARLY -> periodStart.plusMonths(6).minusDays(1);
            case YEARLY -> periodStart.plusYears(1).minusDays(1);
            default -> periodStart.plusMonths(1).minusDays(1);
        };
    }

    // ---- Inner aggregate classes ----

    private static class StatusAggregate {
        long wonCount = 0, lostCount = 0, inProgressCount = 0;
        BigDecimal wonValue = BigDecimal.ZERO, lostValue = BigDecimal.ZERO, inProgressValue = BigDecimal.ZERO;

        void add(DealStatus status, BigDecimal value) {
            if (status == DealStatus.WON) { wonCount++; wonValue = wonValue.add(value); }
            else if (status == DealStatus.LOST) { lostCount++; lostValue = lostValue.add(value); }
            else if (status == DealStatus.IN_PROGRESS) { inProgressCount++; inProgressValue = inProgressValue.add(value); }
        }
    }

    private static class PipelineRevAggregate {
        final Long pipelineId;
        final String pipelineName;
        long totalDeals = 0;
        BigDecimal totalValue = BigDecimal.ZERO;
        BigDecimal totalCommission = BigDecimal.ZERO;

        PipelineRevAggregate(Pipeline pipeline) {
            this.pipelineId = pipeline.getId();
            this.pipelineName = pipeline.getName();
        }

        void add(BigDecimal value, BigDecimal commission) {
            totalDeals++;
            totalValue = totalValue.add(value);
            totalCommission = totalCommission.add(commission);
        }
    }

    private static class PipelineStatusAggregate {
        final Long pipelineId;
        final String pipelineName;
        long wonCount = 0, lostCount = 0, inProgressCount = 0;
        BigDecimal wonValue = BigDecimal.ZERO, lostValue = BigDecimal.ZERO, inProgressValue = BigDecimal.ZERO;

        PipelineStatusAggregate(Pipeline pipeline) {
            this.pipelineId = pipeline.getId();
            this.pipelineName = pipeline.getName();
        }

        void add(DealStatus status, BigDecimal value) {
            if (status == DealStatus.WON) { wonCount++; wonValue = wonValue.add(value); }
            else if (status == DealStatus.LOST) { lostCount++; lostValue = lostValue.add(value); }
            else if (status == DealStatus.IN_PROGRESS) { inProgressCount++; inProgressValue = inProgressValue.add(value); }
        }
    }

    private static class ActivityAggregate {
        long totalActivities = 0, callCount = 0, totalCallMinutes = 0, meetingCount = 0, totalMeetingMinutes = 0;
    }
}
