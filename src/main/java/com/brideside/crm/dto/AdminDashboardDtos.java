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

