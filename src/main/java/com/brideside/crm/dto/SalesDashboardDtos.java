package com.brideside.crm.dto;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * DTOs for the Sales user personal dashboard (data scoped to the authenticated user only).
 */
public class SalesDashboardDtos {

    /**
     * High-level overview for the logged-in sales user.
     */
    public static class SummaryResponse {
        public Long userId;
        public String userName;
        public String email;

        public Long totalDeals;
        public Long wonDeals;
        public Long lostDeals;
        public Long inProgressDeals;

        public BigDecimal totalDealValue;
        public BigDecimal wonDealValue;
        public BigDecimal lostDealValue;
        public BigDecimal inProgressDealValue;

        public BigDecimal totalCommission;
        public BigDecimal wonValueYtd;

        public Long totalActivities;
        public Long pendingActivities;
    }

    /**
     * Monthly WON/LOST/IN_PROGRESS deal counts and values for a year, scoped to the current user.
     */
    public static class DealStatusMonthlyResponse {
        public Integer year;
        public List<DealStatusMonthRow> months;
    }

    public static class DealStatusMonthRow {
        public Integer month;
        public Long wonCount;
        public BigDecimal wonValue;
        public Long lostCount;
        public BigDecimal lostValue;
        public Long inProgressCount;
        public BigDecimal inProgressValue;
    }

    /**
     * Revenue by pipeline for a date range, scoped to the current user.
     */
    public static class RevenueResponse {
        public LocalDate dateFrom;
        public LocalDate dateTo;
        public Long totalDeals;
        public BigDecimal totalDealValue;
        public BigDecimal totalCommission;
        public List<RevenueByPipelineRow> pipelines;
    }

    public static class RevenueByPipelineRow {
        public Long pipelineId;
        public String pipelineName;
        public Long totalDeals;
        public BigDecimal totalDealValue;
        public BigDecimal totalCommission;
    }

    /**
     * Lost reasons breakdown for the current user's deals.
     */
    public static class LostReasonsResponse {
        public Long totalLostDeals;
        public List<LostReasonRow> reasons;
    }

    public static class LostReasonRow {
        public String reason;
        public Long count;
        public BigDecimal percentage;
    }

    /**
     * Monthly activity summary for the current user.
     */
    public static class ActivityMonthlyResponse {
        public Integer year;
        public List<ActivityMonthRow> months;
    }

    public static class ActivityMonthRow {
        public Integer month;
        public Long totalActivities;
        public Long callCount;
        public Long totalCallMinutes;
        public Long meetingCount;
        public Long totalMeetingMinutes;
    }

    /**
     * Per-pipeline deal status for the current user.
     */
    public static class PipelinePerformanceResponse {
        public List<PipelinePerformanceRow> pipelines;
    }

    public static class PipelinePerformanceRow {
        public Long pipelineId;
        public String pipelineName;
        public Long wonCount;
        public BigDecimal wonValue;
        public Long lostCount;
        public BigDecimal lostValue;
        public Long inProgressCount;
        public BigDecimal inProgressValue;
    }

    /**
     * Sales targets vs actual achievement for the current user, broken down by period.
     */
    public static class TargetVsAchievementResponse {
        public Integer year;
        public List<TargetVsAchievementRow> targets;
    }

    public static class TargetVsAchievementRow {
        public String category;
        public String periodType;
        public LocalDate periodStart;
        public BigDecimal targetAmount;
        public BigDecimal achievedAmount;
        public BigDecimal achievementPercentage;
        public List<String> organizationNames;
    }
}
