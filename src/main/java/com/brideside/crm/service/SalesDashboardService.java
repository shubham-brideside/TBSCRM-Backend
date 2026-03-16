package com.brideside.crm.service;

import com.brideside.crm.dto.SalesDashboardDtos;
import com.brideside.crm.entity.User;

import java.time.LocalDate;

/**
 * Sales / presales dashboard. Every method accepts optional {@code dateFrom}/{@code dateTo}
 * (deal reference dates in range) and optional {@code pipelineId}. When dates are omitted, no time filter.
 */
public interface SalesDashboardService {

    SalesDashboardDtos.SummaryResponse getDashboardSummary(
            User currentUser, LocalDate dateFrom, LocalDate dateTo, Long pipelineId);

    SalesDashboardDtos.DealStatusMonthlyResponse getDealStatusMonthly(
            User currentUser, Integer year, LocalDate dateFrom, LocalDate dateTo, Long pipelineId);

    /** When dateFrom/dateTo null, all-time WON revenue; optional pipelineId. */
    SalesDashboardDtos.RevenueResponse getRevenue(
            User currentUser, LocalDate dateFrom, LocalDate dateTo, Long pipelineId);

    boolean isPipelineOwnedBySalesUser(Long pipelineId, User user);

    SalesDashboardDtos.LostReasonsResponse getLostReasons(
            User currentUser, String category, LocalDate dateFrom, LocalDate dateTo, Long pipelineId);

    SalesDashboardDtos.LostReasonsByOrganizationResponse getLostReasonsByOrganization(
            User currentUser, String category, LocalDate dateFrom, LocalDate dateTo, Long pipelineId);

    SalesDashboardDtos.LostReasonsByPipelineResponse getLostReasonsByPipeline(
            User currentUser, String category, LocalDate dateFrom, LocalDate dateTo, Long pipelineId);

    SalesDashboardDtos.LostDealsByStagePerOrganizationResponse getLostDealsByStagePerOrganization(
            User currentUser, String category, LocalDate dateFrom, LocalDate dateTo, Long pipelineId);

    SalesDashboardDtos.LostDealsByStageResponse getLostDealsByStage(
            User currentUser, String category, LocalDate dateFrom, LocalDate dateTo, Long pipelineId);

    SalesDashboardDtos.ActivityMonthlyResponse getActivityMonthly(
            User currentUser, Integer year, LocalDate dateFrom, LocalDate dateTo, Long pipelineId);

    SalesDashboardDtos.PipelinePerformanceResponse getPipelinePerformance(
            User currentUser, LocalDate dateFrom, LocalDate dateTo, Long pipelineId);

    SalesDashboardDtos.TargetVsAchievementResponse getTargetVsAchievement(
            User currentUser, Integer year, LocalDate dateFrom, LocalDate dateTo, Long pipelineId);
}
