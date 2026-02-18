package com.brideside.crm.dto;

import java.math.BigDecimal;
import java.util.List;

/**
 * DTOs for Admin-specific dashboard views.
 */
public class AdminDashboardDtos {

    /**
     * Response wrapper for WON deals grouped by SALES users (all time).
     */
    public static class WonDealsBySalesUserResponse {
        public List<SalesUserWonDealsRow> users;
    }

    /**
     * Aggregated WON deals metrics for a single SALES user (all time).
     */
    public static class SalesUserWonDealsRow {
        public Long userId;
        public String userName;
        public String email;
        public Long totalDeals;
        public BigDecimal totalDealValue;
    }

    /**
     * Response wrapper for LOST deals grouped by SALES users (all time).
     */
    public static class LostDealsBySalesUserResponse {
        public List<SalesUserLostDealsRow> users;
    }

    /**
     * Aggregated LOST deals metrics for a single SALES user (all time).
     */
    public static class SalesUserLostDealsRow {
        public Long userId;
        public String userName;
        public String email;
        public Long totalDeals;
        public BigDecimal totalDealValue;
    }

    /**
     * Response wrapper for won deals grouped by SALES users and month.
     */
    public static class MonthlyWonDealsBySalesUserResponse {
        public Integer year;
        /**
         * Optional organization category filter that was applied, e.g. "Photography".
         * Null if no category filter was used.
         */
        public String category;
        public List<SalesUserMonthlyWonDealsRow> users;
    }

    /**
     * Response wrapper for LOST deals grouped by SALES users and month.
     */
    public static class MonthlyLostDealsBySalesUserResponse {
        public Integer year;
        /**
         * Optional organization category filter that was applied, e.g. "Photography".
         * Null if no category filter was used.
         */
        public String category;
        public List<SalesUserMonthlyLostDealsRow> users;
    }

    /**
     * Per-user monthly won deals statistics for a specific year.
     */
    public static class SalesUserMonthlyWonDealsRow {
        public Long userId;
        public String userName;
        public String email;
        public List<MonthlyDealStats> months;
    }

    /**
     * Per-user monthly LOST deals statistics for a specific year.
     */
    public static class SalesUserMonthlyLostDealsRow {
        public Long userId;
        public String userName;
        public String email;
        public List<MonthlyDealStats> months;
    }

    /**
     * Monthly aggregate for a single user.
     */
    public static class MonthlyDealStats {
        public Integer month; // 1-12
        public Long totalDeals;
        public BigDecimal totalDealValue;
        /**
         * Total commission amount for the deals in this month (may be null or 0 if no commission recorded).
         */
        public BigDecimal totalDealCommission;
    }

    /**
     * Monthly status summary for all deals (not per user).
     */
    public static class DealStatusMonthlySummaryResponse {
        public Integer year;
        public java.util.List<DealStatusMonthlyRow> months;
    }

    public static class DealStatusMonthlyRow {
        public Integer month; // 1-12
        public Long wonCount;
        public BigDecimal wonValue;
        public Long lostCount;
        public BigDecimal lostValue;
        public Long inProgressCount;
        public BigDecimal inProgressValue;
        /**
         * Optional breakdown by organization category for this month.
         */
        public java.util.List<DealStatusCategoryRow> categories;
        /**
         * Optional breakdown by SALES user for this month.
         */
        public java.util.List<DealStatusMonthlyUserRow> users;
        /**
         * Optional breakdown by pipeline for this month.
         */
        public java.util.List<DealStatusPipelineRow> pipelines;
    }

    /**
     * Per-category deal status summary for a given month.
     */
    public static class DealStatusCategoryRow {
        public String category; // e.g. "Photography", "Makeup"
        public Long wonCount;
        public BigDecimal wonValue;
        public Long lostCount;
        public BigDecimal lostValue;
        public Long inProgressCount;
        public BigDecimal inProgressValue;
    }

    /**
     * Per-SALES-user deal status summary for a given month.
     */
    public static class DealStatusMonthlyUserRow {
        public Long userId;
        public String userName;
        public String email;
        public Long wonCount;
        public BigDecimal wonValue;
        public Long lostCount;
        public BigDecimal lostValue;
        public Long inProgressCount;
        public BigDecimal inProgressValue;
    }

    /**
     * Per-pipeline deal status summary for a given month.
     */
    public static class DealStatusPipelineRow {
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
     * Deal status monthly summary grouped by sales user (user-first view).
     * Each user has 12 months of won/lost/in-progress counts and values.
     */
    public static class DealStatusMonthlyByUserResponse {
        public Integer year;
        public java.util.List<DealStatusMonthlyByUserRow> users;
    }

    public static class DealStatusMonthlyByUserRow {
        public Long userId;
        public String userName;
        public String email;
        public java.util.List<DealStatusMonthlyUserMonthRow> months;
    }

    public static class DealStatusMonthlyUserMonthRow {
        public Integer month; // 1-12
        public Long wonCount;
        public BigDecimal wonValue;
        public Long lostCount;
        public BigDecimal lostValue;
        public Long inProgressCount;
        public BigDecimal inProgressValue;
    }

    /**
     * Deal status monthly summary grouped by pipeline (pipeline-first view).
     * Each pipeline has 12 months of won/lost/in-progress counts and values.
     */
    public static class DealStatusMonthlyByPipelineResponse {
        public Integer year;
        public java.util.List<DealStatusMonthlyByPipelineRow> pipelines;
    }

    public static class DealStatusMonthlyByPipelineRow {
        public Long pipelineId;
        public String pipelineName;
        public java.util.List<DealStatusMonthlyUserMonthRow> months;
    }

    /**
     * Monthly activity summary per user (for admin dashboard).
     */
    public static class UserActivityMonthlySummaryResponse {
        public Integer year;
        public java.util.List<UserActivityMonthlyRow> users;
    }

    public static class UserActivityMonthlyRow {
        public Long userId;
        public String userName;
        public String email;
        public java.util.List<UserActivityMonthStats> months;
    }

    public static class UserActivityMonthStats {
        public Integer month; // 1-12
        public Long totalActivities;
        public Long callCount;
        public Long totalCallMinutes;
        public Long meetingCount;
        public Long totalMeetingMinutes;
    }

    /**
     * Summary of LOST deal reasons (for donut chart).
     */
    public static class LostReasonSummaryResponse {
        public Long totalLostDeals;
        /**
         * Optional organization category filter that was applied, e.g. "Photography".
         * Null if no category filter was used.
         */
        public String category;
        /**
         * Optional SALES user filter that was applied (attributed via pipeline -> organization -> owner).
         * Null if no user filter was used.
         */
        public Long userId;
        /**
         * Optional pipeline filter that was applied.
         * Null if no pipeline filter was used.
         */
        public Long pipelineId;
        public java.util.List<LostReasonRow> reasons;
    }

    public static class LostReasonRow {
        public String reason;          // display name, e.g. "Budget"
        public Long count;            // number of LOST deals with this reason
        public BigDecimal percentage; // percentage of total lost deals (0–100)
    }

    /**
     * Lost reasons grouped by pipeline (each pipeline has its own reason breakdown).
     */
    public static class LostReasonsByPipelineResponse {
        /**
         * Optional organization category filter applied. Null if none.
         */
        public String category;
        public java.util.List<LostReasonsByPipelineRow> pipelines;
    }

    public static class LostReasonsByPipelineRow {
        public Long pipelineId;
        public String pipelineName;
        public Long totalLostDeals;
        public java.util.List<LostReasonRow> reasons;
    }

    /**
     * Lost reasons grouped by sales user (each user has their own reason breakdown).
     */
    public static class LostReasonsByUserResponse {
        /**
         * Optional organization category filter applied. Null if none.
         */
        public String category;
        public java.util.List<LostReasonsByUserRow> users;
    }

    public static class LostReasonsByUserRow {
        public Long userId;
        public String userName;
        public String email;
        public Long totalLostDeals;
        public java.util.List<LostReasonRow> reasons;
    }

    /**
     * Deal status summary per organization (all time + per month for a year).
     */
    public static class OrganizationDealStatusSummaryResponse {
        public Integer year;
        public java.util.List<OrganizationDealStatusRow> organizations;
    }

    public static class OrganizationDealStatusRow {
        public Long organizationId;
        public String organizationName;
        public String organizationCategory;
        // Optional owner (SALES user or general owner) of this organization
        public Long ownerId;
        public String ownerName;
        public String ownerEmail;

        // All-time totals
        public Long wonCountAll;
        public BigDecimal wonValueAll;
        public Long lostCountAll;
        public BigDecimal lostValueAll;
        public Long inProgressCountAll;
        public BigDecimal inProgressValueAll;

        // Monthly breakdown for the requested year
        public java.util.List<OrganizationDealStatusMonthlyRow> months;
    }

    public static class OrganizationDealStatusMonthlyRow {
        public Integer month; // 1-12
        public Long wonCount;
        public BigDecimal wonValue;
        public Long lostCount;
        public BigDecimal lostValue;
        public Long inProgressCount;
        public BigDecimal inProgressValue;
    }

    /**
     * Sales users and the pipelines they own (via organization -> owner).
     * Used by dashboards to populate filters like "Sales → Pipelines".
     */
    public static class SalesPipelinesResponse {
        public java.util.List<SalesPipelinesRow> users;
    }

    public static class SalesPipelinesRow {
        public Long userId;
        public String userName;
        public String email;
        public java.util.List<PipelineRef> pipelines;
    }

    public static class PipelineRef {
        public Long pipelineId;
        public String pipelineName;
    }

    /**
     * Deal divert report: all WON diverted deals with from/to pipeline and owner.
     * Supports time filtering: all-time totals + optional monthly breakdown by year.
     */
    public static class DealDivertReportResponse {
        /** All-time totals (all diverted deals). */
        public DealDivertReportAllTime allTime;
        /** Optional monthly breakdown (if year parameter provided). */
        public DealDivertReportMonthly monthly;
    }

    public static class DealDivertReportAllTime {
        public java.util.List<DealDivertReportRow> deals;
        public Long totalCount;
        public BigDecimal totalValue;
    }

    public static class DealDivertReportMonthly {
        public Integer year;
        public java.util.List<DealDivertReportMonthRow> months;
    }

    public static class DealDivertReportMonthRow {
        public Integer month; // 1-12
        public java.util.List<DealDivertReportRow> deals;
        public Long count;
        public BigDecimal value;
    }

    public static class DealDivertReportRow {
        public Long dealId;
        public String dealName;
        public BigDecimal dealValue;
        public java.time.LocalDateTime wonAt;
        /** Pipeline from which the deal was diverted (source). */
        public Long divertedFromPipelineId;
        public String divertedFromPipelineName;
        /** Pipeline to which the deal was diverted (current pipeline). */
        public Long divertedToPipelineId;
        public String divertedToPipelineName;
        /** Owner of the deal (current pipeline's organization owner). */
        public Long ownerId;
        public String ownerName;
        public String ownerEmail;
        public String organizationName;
        /** Original deal ID if this deal was created by diversion. */
        public Long referencedDealId;
        /** User who diverted the deal (creator of the diverted deal). */
        public Long divertedByUserId;
        public String divertedByUserName;
        public String divertedByUserEmail;
        /** User who receives the diverted deal (current pipeline's organization owner). */
        public Long divertedToUserId;
        public String divertedToUserName;
        public String divertedToUserEmail;
    }

    /**
     * Revenue summary over a time range (for admin dashboard).
     * Uses WON, non-deleted deals only.
     */
    public static class RevenueSummaryResponse {
        public java.time.LocalDate dateFrom;
        public java.time.LocalDate dateTo;
        public Long totalDeals;
        public BigDecimal totalDealValue;
        public java.util.List<RevenueByCategoryRow> categories;
        public java.util.List<RevenueByUserRow> users;
        public java.util.List<RevenueByPipelineRow> pipelines;
    }

    public static class RevenueByCategoryRow {
        public String category; // organization category, e.g. "Photography"
        public Long totalDeals;
        public BigDecimal totalDealValue;
    }

    public static class RevenueByUserRow {
        public Long userId;
        public String userName;
        public String email;
        public Long totalDeals;
        public BigDecimal totalDealValue;
    }

    public static class RevenueByPipelineRow {
        public Long pipelineId;
        public String pipelineName;
        public Long totalDeals;
        public BigDecimal totalDealValue;
    }
}

