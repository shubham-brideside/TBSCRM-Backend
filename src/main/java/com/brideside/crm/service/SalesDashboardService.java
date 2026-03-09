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

    SalesDashboardDtos.LostReasonsResponse getLostReasons(User currentUser);

    SalesDashboardDtos.ActivityMonthlyResponse getActivityMonthly(User currentUser, Integer year);

    SalesDashboardDtos.PipelinePerformanceResponse getPipelinePerformance(User currentUser);

    SalesDashboardDtos.TargetVsAchievementResponse getTargetVsAchievement(User currentUser, Integer year);
}
