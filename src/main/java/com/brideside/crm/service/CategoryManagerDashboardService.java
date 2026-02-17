package com.brideside.crm.service;

import com.brideside.crm.dto.AdminDashboardDtos;
import com.brideside.crm.dto.CategoryManagerDashboardDtos;
import com.brideside.crm.entity.User;

import java.time.LocalDate;

/**
 * Dashboard service for Category Manager role. Returns admin-style metrics
 * scoped to the Category Manager's hierarchy (their Sales/Presales reports and own data).
 */
public interface CategoryManagerDashboardService {

    /**
     * Summary stats for the Category Manager dashboard (team counts + deal totals).
     */
    CategoryManagerDashboardDtos.SummaryResponse getDashboardSummary(User categoryManager);

    /**
     * Revenue summary (WON deals) by category, user, and pipeline, scoped to this Category Manager.
     */
    AdminDashboardDtos.RevenueSummaryResponse getRevenueSummary(User categoryManager,
            LocalDate dateFrom, LocalDate dateTo);

    /**
     * Won deals grouped by SALES user (and Category Manager), scoped to this Category Manager.
     */
    AdminDashboardDtos.WonDealsBySalesUserResponse getWonDealsBySalesUser(User categoryManager);

    /**
     * Lost deals grouped by SALES user (and Category Manager), scoped to this Category Manager.
     */
    AdminDashboardDtos.LostDealsBySalesUserResponse getLostDealsBySalesUser(User categoryManager);

    /**
     * Monthly deal status summary (WON/LOST/IN_PROGRESS) for the year, scoped to this Category Manager.
     */
    AdminDashboardDtos.DealStatusMonthlySummaryResponse getDealStatusMonthlySummary(
            User categoryManager, Integer year);

    /**
     * Deal status monthly summary per sales user (user-first: each user with 12 months of counts/values),
     * scoped to this Category Manager.
     */
    AdminDashboardDtos.DealStatusMonthlyByUserResponse getDealStatusMonthlyByUser(
            User categoryManager, Integer year);

    /**
     * Deal status monthly summary per pipeline (pipeline-first: each pipeline with 12 months of counts/values),
     * scoped to this Category Manager.
     */
    AdminDashboardDtos.DealStatusMonthlyByPipelineResponse getDealStatusMonthlyByPipeline(
            User categoryManager, Integer year);

    /**
     * Lost deal reasons summary, scoped to this Category Manager. Optional filters (category, userId, pipelineId).
     */
    AdminDashboardDtos.LostReasonSummaryResponse getLostReasonSummary(User categoryManager,
            String category, Long userId, Long pipelineId);

    /**
     * Lost reasons grouped by pipeline; each pipeline has its own reason breakdown. Optional category filter.
     */
    AdminDashboardDtos.LostReasonsByPipelineResponse getLostReasonsByPipeline(User categoryManager,
            String category);

    /**
     * Lost reasons grouped by sales user; each user has their own reason breakdown. Optional category filter.
     */
    AdminDashboardDtos.LostReasonsByUserResponse getLostReasonsByUser(User categoryManager,
            String category);

    /**
     * Monthly WON deals (including commission) grouped by SALES user, scoped to this Category Manager.
     * Same shape as admin's MonthlyWonDealsBySalesUserResponse.
     */
    AdminDashboardDtos.MonthlyWonDealsBySalesUserResponse getWonDealsBySalesUserMonthly(
            User categoryManager, Integer year, String category, LocalDate dateFrom, LocalDate dateTo);

    /**
     * List SALES users in this Category Manager's hierarchy and the pipelines they own
     * (via pipeline.organization.owner).
     */
    AdminDashboardDtos.SalesPipelinesResponse getSalesUsersWithPipelines(User categoryManager);

    /**
     * Monthly activity summary per user (calls, meetings, total activities) for the given year,
     * scoped to this Category Manager's hierarchy.
     */
    AdminDashboardDtos.UserActivityMonthlySummaryResponse getUserActivityMonthlySummary(
            User categoryManager, Integer year);

    /**
     * Deal status summary per organization (all time + per month for a year), scoped to this Category Manager.
     * Only includes organizations owned by users in the Category Manager's hierarchy.
     */
    AdminDashboardDtos.OrganizationDealStatusSummaryResponse getOrganizationDealStatusSummary(
            User categoryManager, Integer year);
}
