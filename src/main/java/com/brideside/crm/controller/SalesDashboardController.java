package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.SalesDashboardDtos;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.UnauthorizedException;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.SalesDashboardService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;

@RestController
@RequestMapping("/api/sales/dashboard")
@Tag(name = "Sales Dashboard", description = "Personal dashboard for the authenticated sales user — all data is scoped to the current user only")
public class SalesDashboardController {

    private final SalesDashboardService salesDashboardService;
    private final UserRepository userRepository;

    public SalesDashboardController(SalesDashboardService salesDashboardService,
                                    UserRepository userRepository) {
        this.salesDashboardService = salesDashboardService;
        this.userRepository = userRepository;
    }

    @GetMapping("/summary")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Dashboard summary",
            description = "High-level overview: deal counts, values, commission, YTD won, and activity stats for the authenticated user.")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.SummaryResponse>> getSummary() {
        User user = getCurrentUser();
        SalesDashboardDtos.SummaryResponse data = salesDashboardService.getDashboardSummary(user);
        return ResponseEntity.ok(ApiResponse.success("Dashboard summary fetched", data));
    }

    @GetMapping("/deals-status-monthly")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Monthly deal status",
            description = "WON/LOST/IN_PROGRESS counts and values per month for a given year, for the authenticated user.")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.DealStatusMonthlyResponse>> getDealStatusMonthly(
            @RequestParam("year") Integer year) {
        if (year == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("year is required"));
        }
        User user = getCurrentUser();
        SalesDashboardDtos.DealStatusMonthlyResponse data =
                salesDashboardService.getDealStatusMonthly(user, year);
        return ResponseEntity.ok(ApiResponse.success("Deal status monthly fetched", data));
    }

    @GetMapping("/revenue")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Revenue by pipeline",
            description = "WON deals revenue in the given date range, broken down by pipeline, for the authenticated user.")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.RevenueResponse>> getRevenue(
            @RequestParam("dateFrom") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam("dateTo") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo) {
        if (dateFrom == null || dateTo == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("dateFrom and dateTo are required"));
        }
        User user = getCurrentUser();
        SalesDashboardDtos.RevenueResponse data = salesDashboardService.getRevenue(user, dateFrom, dateTo);
        return ResponseEntity.ok(ApiResponse.success("Revenue fetched", data));
    }

    @GetMapping("/lost-reasons")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Lost deal reasons",
            description = "Breakdown of lost deal reasons with counts and percentages, scoped to the authenticated user only "
                    + "(deal → pipeline → organization → owner). Optional filters match Category Manager: "
                    + "category = organization category (e.g. Photography), pipelineId = pipeline.")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.LostReasonsResponse>> getLostReasons(
            @RequestParam(value = "category", required = false) String category,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        User user = getCurrentUser();
        if (pipelineId != null && !salesDashboardService.isPipelineOwnedBySalesUser(pipelineId, user)) {
            return ResponseEntity.badRequest().body(
                    ApiResponse.error("pipelineId must be a pipeline belonging to your organization"));
        }
        SalesDashboardDtos.LostReasonsResponse data =
                salesDashboardService.getLostReasons(user, category, pipelineId);
        return ResponseEntity.ok(ApiResponse.success("Lost reasons fetched", data));
    }

    @GetMapping("/lost-reasons-by-organization")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Lost reasons per organization",
            description = "LOST deal reasons grouped by organization for the authenticated user only. "
                    + "Optional category = organization category filter (same as Category Manager).")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.LostReasonsByOrganizationResponse>> getLostReasonsByOrganization(
            @RequestParam(value = "category", required = false) String category) {
        User user = getCurrentUser();
        SalesDashboardDtos.LostReasonsByOrganizationResponse data =
                salesDashboardService.getLostReasonsByOrganization(user, category);
        return ResponseEntity.ok(ApiResponse.success("Lost reasons by organization fetched", data));
    }

    @GetMapping("/lost-reasons-by-pipeline")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Lost reasons per pipeline",
            description = "LOST deal reasons grouped by pipeline for the authenticated user only. "
                    + "Optional category = organization category filter.")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.LostReasonsByPipelineResponse>> getLostReasonsByPipeline(
            @RequestParam(value = "category", required = false) String category,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        User user = getCurrentUser();
        if (pipelineId != null && !salesDashboardService.isPipelineOwnedBySalesUser(pipelineId, user)) {
            return ResponseEntity.badRequest().body(
                    ApiResponse.error("pipelineId must be a pipeline belonging to your organization"));
        }
        SalesDashboardDtos.LostReasonsByPipelineResponse data =
                salesDashboardService.getLostReasonsByPipeline(user, category, pipelineId);
        return ResponseEntity.ok(ApiResponse.success("Lost reasons by pipeline fetched", data));
    }

    @GetMapping("/lost-deals-by-stage-per-organization")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Lost deals by stage per organization",
            description = "LOST deals grouped by organization and by stage (pipeline stage at time of reporting), "
                    + "with count and value per stage. Scoped to the authenticated user only. "
                    + "Optional category (org category) and pipelineId (must be your pipeline).")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.LostDealsByStagePerOrganizationResponse>> getLostDealsByStagePerOrganization(
            @RequestParam(value = "category", required = false) String category,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        User user = getCurrentUser();
        if (pipelineId != null && !salesDashboardService.isPipelineOwnedBySalesUser(pipelineId, user)) {
            return ResponseEntity.badRequest().body(
                    ApiResponse.error("pipelineId must be a pipeline belonging to your organization"));
        }
        SalesDashboardDtos.LostDealsByStagePerOrganizationResponse data =
                salesDashboardService.getLostDealsByStagePerOrganization(user, category, pipelineId);
        return ResponseEntity.ok(ApiResponse.success("Lost deals by stage per organization fetched", data));
    }

    @GetMapping("/activities-monthly")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Monthly activity summary",
            description = "Monthly activity counts (calls, meetings, durations) for the authenticated user.")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.ActivityMonthlyResponse>> getActivityMonthly(
            @RequestParam("year") Integer year) {
        if (year == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("year is required"));
        }
        User user = getCurrentUser();
        SalesDashboardDtos.ActivityMonthlyResponse data =
                salesDashboardService.getActivityMonthly(user, year);
        return ResponseEntity.ok(ApiResponse.success("Activity monthly summary fetched", data));
    }

    @GetMapping("/pipeline-performance")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Pipeline performance",
            description = "Per-pipeline deal status (WON/LOST/IN_PROGRESS counts and values) for the authenticated user.")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.PipelinePerformanceResponse>> getPipelinePerformance() {
        User user = getCurrentUser();
        SalesDashboardDtos.PipelinePerformanceResponse data =
                salesDashboardService.getPipelinePerformance(user);
        return ResponseEntity.ok(ApiResponse.success("Pipeline performance fetched", data));
    }

    @GetMapping("/target-vs-achievement")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Target vs achievement",
            description = "Sales targets compared against actual WON deal revenue for a given year, for the authenticated user.")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.TargetVsAchievementResponse>> getTargetVsAchievement(
            @RequestParam("year") Integer year) {
        if (year == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("year is required"));
        }
        User user = getCurrentUser();
        SalesDashboardDtos.TargetVsAchievementResponse data =
                salesDashboardService.getTargetVsAchievement(user, year);
        return ResponseEntity.ok(ApiResponse.success("Target vs achievement fetched", data));
    }

    private User getCurrentUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !(authentication.getPrincipal() instanceof UserDetails)) {
            throw new UnauthorizedException("User not authenticated");
        }
        String email = ((UserDetails) authentication.getPrincipal()).getUsername();
        return userRepository.findByEmail(email)
                .orElseThrow(() -> new UnauthorizedException("Current user not found"));
    }
}
