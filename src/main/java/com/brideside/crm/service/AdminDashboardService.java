package com.brideside.crm.service;

import com.brideside.crm.dto.AdminDashboardDtos;

public interface AdminDashboardService {

    /**
     * Aggregate WON deals grouped by SALES users using the pipeline -> organization -> owner chain.
     */
    AdminDashboardDtos.WonDealsBySalesUserResponse getWonDealsBySalesUser();

    /**
     * Aggregate WON deals grouped by SALES users and month for a specific year.
     */
    AdminDashboardDtos.MonthlyWonDealsBySalesUserResponse getWonDealsBySalesUserMonthly(Integer year);

    /**
     * Aggregate LOST deals grouped by SALES users using the pipeline -> organization -> owner chain.
     */
    AdminDashboardDtos.LostDealsBySalesUserResponse getLostDealsBySalesUser();

    /**
     * Aggregate LOST deals grouped by SALES users and month for a specific year.
     */
    AdminDashboardDtos.MonthlyLostDealsBySalesUserResponse getLostDealsBySalesUserMonthly(Integer year);

    /**
     * Aggregate WON/LOST/IN_PROGRESS deals by month for a specific year (global, not per user).
     */
    AdminDashboardDtos.DealStatusMonthlySummaryResponse getDealStatusMonthlySummary(Integer year);

    /**
     * Aggregate activities per user per month for a specific year, including
     * total activity count and call count/duration.
     */
    AdminDashboardDtos.UserActivityMonthlySummaryResponse getUserActivityMonthlySummary(Integer year);

    /**
     * Aggregate WON/LOST/IN_PROGRESS deals per organization, returning both
     * all-time totals and per-month aggregates for a given year.
     */
    AdminDashboardDtos.OrganizationDealStatusSummaryResponse getOrganizationDealStatusSummary(Integer year);

    /**
     * Aggregate LOST deals by lostReason (all time), returning counts and percentages.
     */
    AdminDashboardDtos.LostReasonSummaryResponse getLostReasonSummary();
}

