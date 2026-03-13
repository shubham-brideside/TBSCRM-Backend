package com.brideside.crm.service;

import com.brideside.crm.dto.SalesDashboardDtos;
import com.brideside.crm.entity.User;

import java.time.LocalDate;

/**
 * Dashboard service for the Sales role. All data is scoped to the authenticated user's
 * own organizations (deal -> pipeline -> organization -> owner == currentUser).
 */
public interface SalesDashboardService {

    SalesDashboardDtos.SummaryResponse getDashboardSummary(User currentUser);

    SalesDashboardDtos.DealStatusMonthlyResponse getDealStatusMonthly(User currentUser, Integer year);

    SalesDashboardDtos.RevenueResponse getRevenue(User currentUser, LocalDate dateFrom, LocalDate dateTo);

    /**
     * True if the pipeline exists, is not deleted, and belongs to an organization owned by {@code user}.
     */
    boolean isPipelineOwnedBySalesUser(Long pipelineId, User user);

    /**
     * Lost reasons for the current user's LOST deals only.
     * Optional {@code category} filters by organization category; {@code pipelineId} must be a pipeline
     * owned by the user (same as deal → pipeline → organization → owner).
     */
    SalesDashboardDtos.LostReasonsResponse getLostReasons(User currentUser, String category, Long pipelineId);

    SalesDashboardDtos.LostReasonsByOrganizationResponse getLostReasonsByOrganization(User currentUser, String category);

    /**
     * Lost reasons per pipeline for pipelines the user owns (via organization owner).
     * If {@code pipelineId} is set, only that pipeline is returned (must be owned by the user).
     */
    SalesDashboardDtos.LostReasonsByPipelineResponse getLostReasonsByPipeline(
            User currentUser, String category, Long pipelineId);

    /**
     * LOST deal counts and values by stage, grouped per organization (user-owned deals only).
     */
    SalesDashboardDtos.LostDealsByStagePerOrganizationResponse getLostDealsByStagePerOrganization(
            User currentUser, String category, Long pipelineId);

    SalesDashboardDtos.ActivityMonthlyResponse getActivityMonthly(User currentUser, Integer year);

    SalesDashboardDtos.PipelinePerformanceResponse getPipelinePerformance(User currentUser);

    SalesDashboardDtos.TargetVsAchievementResponse getTargetVsAchievement(User currentUser, Integer year);
}
