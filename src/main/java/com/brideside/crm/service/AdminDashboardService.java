package com.brideside.crm.service;

import com.brideside.crm.dto.AdminDashboardDtos;

import java.time.LocalDate;

public interface AdminDashboardService {

    /**
     * Aggregate WON deals grouped by SALES users using the pipeline -> organization -> owner chain.
     */
    AdminDashboardDtos.WonDealsBySalesUserResponse getWonDealsBySalesUser();

    /**
     * Aggregate WON deals grouped by SALES users and month for a specific year.
     */
    AdminDashboardDtos.MonthlyWonDealsBySalesUserResponse getWonDealsBySalesUserMonthly(
            Integer year,
            String category,
            LocalDate dateFrom,
            LocalDate dateTo
    );

    /**
     * Aggregate LOST deals grouped by SALES users using the pipeline -> organization -> owner chain.
     */
    AdminDashboardDtos.LostDealsBySalesUserResponse getLostDealsBySalesUser();

    /**
     * Aggregate LOST deals grouped by SALES users and month for a specific year.
     */
    AdminDashboardDtos.MonthlyLostDealsBySalesUserResponse getLostDealsBySalesUserMonthly(
            Integer year,
            String category,
            LocalDate dateFrom,
            LocalDate dateTo
    );

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
    AdminDashboardDtos.LostReasonSummaryResponse getLostReasonSummary(
            String category,
            Long userId,
            Long pipelineId
    );

    /**
     * Revenue summary (WON, non-deleted deals) over a time range,
     * aggregated per category, per SALES user, and per pipeline.
     */
    AdminDashboardDtos.RevenueSummaryResponse getRevenueSummary(
            java.time.LocalDate dateFrom,
            java.time.LocalDate dateTo
    );

    /**
     * Deal divert report: all WON diverted deals with diverted-from pipeline, diverted-to pipeline, and owner.
     * Returns all-time totals + optional monthly breakdown if year provided.
     */
    AdminDashboardDtos.DealDivertReportResponse getDealDivertReport(Integer year);
}

