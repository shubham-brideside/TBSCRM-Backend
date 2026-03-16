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

        /** Applied filters (echo). */
        public LocalDate dateFrom;
        public LocalDate dateTo;
        public Long pipelineId;
    }

    /**
     * Monthly WON/LOST/IN_PROGRESS deal counts and values for a year, scoped to the current user.
     */
    public static class DealStatusMonthlyResponse {
        public Integer year;
        public List<DealStatusMonthRow> months;
        public LocalDate dateFrom;
        public LocalDate dateTo;
        public Long pipelineId;
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
        public Long pipelineId;
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
     * Optional {@code category} / {@code pipelineId} echo filters when used (same semantics as Category Manager dashboard).
     */
    public static class LostReasonsResponse {
        public Long totalLostDeals;
        /**
         * Organization category filter applied, e.g. "Photography". Null if none.
         */
        public String category;
        /**
         * Pipeline filter applied. Null if none.
         */
        public Long pipelineId;
        public List<LostReasonRow> reasons;
        public LocalDate dateFrom;
        public LocalDate dateTo;
    }

    /**
     * Lost reasons grouped by organization (each org owned by the sales user has its own breakdown).
     */
    public static class LostReasonsByOrganizationResponse {
        public String category;
        public List<LostReasonsByOrganizationRow> organizations;
        public LocalDate dateFrom;
        public LocalDate dateTo;
        public Long pipelineId;
    }

    public static class LostReasonsByOrganizationRow {
        public Long organizationId;
        public String organizationName;
        public String organizationCategory;
        public Long totalLostDeals;
        public List<LostReasonRow> reasons;
    }

    /**
     * Lost reasons grouped by pipeline for the current user (optional org category filter).
     */
    public static class LostReasonsByPipelineResponse {
        public String category;
        public List<LostReasonsByPipelineRow> pipelines;
        public LocalDate dateFrom;
        public LocalDate dateTo;
        public Long pipelineId;
    }

    public static class LostReasonsByPipelineRow {
        public Long pipelineId;
        public String pipelineName;
        public Long totalLostDeals;
        public List<LostReasonRow> reasons;
    }

    public static class LostReasonRow {
        public String reason;
        public Long count;
        public BigDecimal percentage;
    }

    /**
     * LOST deals grouped by organization, then by pipeline stage (where the deal was when marked lost).
     * Scoped to the sales user (deal → pipeline → organization → owner).
     */
    public static class LostDealsByStagePerOrganizationResponse {
        /** Organization category filter if any */
        public String category;
        public Long pipelineId;
        public List<LostDealsByStageOrganizationRow> organizations;
        public LocalDate dateFrom;
        public LocalDate dateTo;
    }

    public static class LostDealsByStageOrganizationRow {
        public Long organizationId;
        public String organizationName;
        public String organizationCategory;
        public Long totalLostDeals;
        public BigDecimal totalLostValue;
        /** Per stage within this org */
        public List<LostDealsByStageRow> stages;
    }

    public static class LostDealsByStageRow {
        /** Null when deal had no stage at loss time */
        public Long stageId;
        public String stageName;
        public Long pipelineId;
        public String pipelineName;
        public Long lostCount;
        public BigDecimal lostValue;
    }

    /**
     * LOST deals aggregated by pipeline + stage (flat list). Same user scope as other dashboard lost APIs.
     */
    public static class LostDealsByStageResponse {
        public String category;
        public Long pipelineId;
        public Long totalLostDeals;
        public BigDecimal totalLostValue;
        public List<LostDealsByStageRow> stages;
        public LocalDate dateFrom;
        public LocalDate dateTo;
    }

    /**
     * Monthly activity summary for the current user.
     */
    public static class ActivityMonthlyResponse {
        public Integer year;
        public List<ActivityMonthRow> months;
        public LocalDate dateFrom;
        public LocalDate dateTo;
        public Long pipelineId;
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
        public LocalDate dateFrom;
        public LocalDate dateTo;
        public Long pipelineId;
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
        public LocalDate dateFrom;
        public LocalDate dateTo;
        public Long pipelineId;
    }

    public static class TargetVsAchievementRow {
        public String category;
        public String periodType;
        public LocalDate periodStart;
        /** Calendar month (1–12) for this row — same granularity as Target page month blocks. */
        public Integer month;
        public Integer year;
        public BigDecimal targetAmount;
        /**
         * Same as Target page: SALES = sum of won deal commission in period/category;
         * PRESALES = won deal count (incentive-based view).
         */
        public BigDecimal achievedAmount;
        public BigDecimal achievementPercentage;
        /** COMMISSION (sales) or DEAL_COUNT (presales) — matches Target dashboard semantics. */
        public String achievedBasis;
        /** Same slabs as Target page (SALES): % applied to achieved commission. PRESALES: 0 with fixed incentiveAmount. */
        public BigDecimal incentivePercent;
        /** SALES: incentivePercent × achieved (commission). PRESALES: 500×direct + 1000×divert won deals. */
        public BigDecimal incentiveAmount;
        public List<String> organizationNames;
    }
}
