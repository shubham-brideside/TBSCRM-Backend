package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.AdminDashboardDtos;
import com.brideside.crm.dto.CategoryManagerDashboardDtos;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.UnauthorizedException;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.CategoryManagerDashboardService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;

@RestController
@RequestMapping("/api/category-manager/dashboard")
@Tag(name = "Category Manager Dashboard", description = "Admin-style dashboard scoped to Category Manager's hierarchy (category level)")
public class CategoryManagerDashboardController {

    @Autowired
    private CategoryManagerDashboardService categoryManagerDashboardService;

    @Autowired
    private UserRepository userRepository;

    @GetMapping("/summary")
    @PreAuthorize("hasRole('CATEGORY_MANAGER')")
    @Operation(summary = "Dashboard summary", description = "Category-level summary: team counts and deal totals for the Category Manager's hierarchy.")
    public ResponseEntity<ApiResponse<CategoryManagerDashboardDtos.SummaryResponse>> getDashboardSummary() {
        User currentUser = getCurrentCategoryManager();
        CategoryManagerDashboardDtos.SummaryResponse data =
                categoryManagerDashboardService.getDashboardSummary(currentUser);
        return ResponseEntity.ok(ApiResponse.success("Dashboard summary fetched", data));
    }

    @GetMapping("/revenue")
    @PreAuthorize("hasRole('CATEGORY_MANAGER')")
    @Operation(summary = "Revenue by category, user, pipeline", description = "WON deals in date range, scoped to Category Manager's hierarchy, broken down by organization category, user, and pipeline.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.RevenueSummaryResponse>> getRevenueSummary(
            @RequestParam("dateFrom") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam("dateTo") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo) {
        if (dateFrom == null || dateTo == null) {
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("dateFrom and dateTo are required"));
        }
        User currentUser = getCurrentCategoryManager();
        AdminDashboardDtos.RevenueSummaryResponse data =
                categoryManagerDashboardService.getRevenueSummary(currentUser, dateFrom, dateTo);
        return ResponseEntity.ok(ApiResponse.success("Revenue summary fetched", data));
    }

    @GetMapping("/won-deals-by-sales-user")
    @PreAuthorize("hasRole('CATEGORY_MANAGER')")
    @Operation(summary = "Won deals by user", description = "WON deals grouped by SALES/Category Manager (organization owner), scoped to this Category Manager.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.WonDealsBySalesUserResponse>> getWonDealsBySalesUser() {
        User currentUser = getCurrentCategoryManager();
        AdminDashboardDtos.WonDealsBySalesUserResponse data =
                categoryManagerDashboardService.getWonDealsBySalesUser(currentUser);
        return ResponseEntity.ok(ApiResponse.success("Won deals by user fetched", data));
    }

    @GetMapping("/won-deals-by-sales-user/monthly")
    @PreAuthorize("hasRole('CATEGORY_MANAGER')")
    @Operation(summary = "Monthly won deals by SALES user (with commission)",
            description = "Monthly WON deals grouped by SALES users for the given year, including counts, total value, "
                    + "and total commission per user. Scoped to this Category Manager's hierarchy. "
                    + "Optionally filter by organization category and/or date range within the year.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.MonthlyWonDealsBySalesUserResponse>> getWonDealsBySalesUserMonthly(
            @RequestParam("year") Integer year,
            @RequestParam(value = "category", required = false) String category,
            @RequestParam(value = "dateFrom", required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(value = "dateTo", required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo) {
        if (year == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("year is required"));
        }
        User currentUser = getCurrentCategoryManager();
        AdminDashboardDtos.MonthlyWonDealsBySalesUserResponse data =
                categoryManagerDashboardService.getWonDealsBySalesUserMonthly(currentUser, year, category, dateFrom, dateTo);
        return ResponseEntity.ok(ApiResponse.success("Monthly won deals grouped by sales user fetched", data));
    }

    @GetMapping("/lost-deals-by-sales-user")
    @PreAuthorize("hasRole('CATEGORY_MANAGER')")
    @Operation(summary = "Lost deals by user", description = "LOST deals grouped by SALES/Category Manager (organization owner), scoped to this Category Manager.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.LostDealsBySalesUserResponse>> getLostDealsBySalesUser() {
        User currentUser = getCurrentCategoryManager();
        AdminDashboardDtos.LostDealsBySalesUserResponse data =
                categoryManagerDashboardService.getLostDealsBySalesUser(currentUser);
        return ResponseEntity.ok(ApiResponse.success("Lost deals by user fetched", data));
    }

    @GetMapping("/deals-status-monthly")
    @PreAuthorize("hasRole('CATEGORY_MANAGER')")
    @Operation(summary = "Monthly deal status", description = "Monthly WON/LOST/IN_PROGRESS counts and values for the year, with category and user breakdown, scoped to this Category Manager.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.DealStatusMonthlySummaryResponse>> getDealStatusMonthlySummary(
            @RequestParam("year") Integer year) {
        if (year == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("year is required"));
        }
        User currentUser = getCurrentCategoryManager();
        AdminDashboardDtos.DealStatusMonthlySummaryResponse data =
                categoryManagerDashboardService.getDealStatusMonthlySummary(currentUser, year);
        return ResponseEntity.ok(ApiResponse.success("Deal status monthly summary fetched", data));
    }

    @GetMapping("/deals-status-monthly-by-user")
    @PreAuthorize("hasRole('CATEGORY_MANAGER')")
    @Operation(summary = "Deal status monthly per sales user", description = "Deal status (WON/LOST/IN_PROGRESS) per sales user with 12 months breakdown for the year, scoped to this Category Manager.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.DealStatusMonthlyByUserResponse>> getDealStatusMonthlyByUser(
            @RequestParam("year") Integer year) {
        if (year == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("year is required"));
        }
        User currentUser = getCurrentCategoryManager();
        AdminDashboardDtos.DealStatusMonthlyByUserResponse data =
                categoryManagerDashboardService.getDealStatusMonthlyByUser(currentUser, year);
        return ResponseEntity.ok(ApiResponse.success("Deal status monthly by user fetched", data));
    }

    @GetMapping("/deals-status-monthly-by-pipeline")
    @PreAuthorize("hasRole('CATEGORY_MANAGER')")
    @Operation(summary = "Deal status monthly per pipeline", description = "Deal status (WON/LOST/IN_PROGRESS) per pipeline with 12 months breakdown for the year, scoped to this Category Manager.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.DealStatusMonthlyByPipelineResponse>> getDealStatusMonthlyByPipeline(
            @RequestParam("year") Integer year) {
        if (year == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("year is required"));
        }
        User currentUser = getCurrentCategoryManager();
        AdminDashboardDtos.DealStatusMonthlyByPipelineResponse data =
                categoryManagerDashboardService.getDealStatusMonthlyByPipeline(currentUser, year);
        return ResponseEntity.ok(ApiResponse.success("Deal status monthly by pipeline fetched", data));
    }

    @GetMapping("/lost-reasons")
    @PreAuthorize("hasRole('CATEGORY_MANAGER')")
    @Operation(summary = "Lost deal reasons", description = "LOST deal reasons with count and percentage, scoped to this Category Manager. Optional filters: category, userId, pipelineId.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.LostReasonSummaryResponse>> getLostReasonSummary(
            @RequestParam(value = "category", required = false) String category,
            @RequestParam(value = "userId", required = false) Long userId,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        User currentUser = getCurrentCategoryManager();
        AdminDashboardDtos.LostReasonSummaryResponse data =
                categoryManagerDashboardService.getLostReasonSummary(currentUser, category, userId, pipelineId);
        return ResponseEntity.ok(ApiResponse.success("Lost reason summary fetched", data));
    }

    @GetMapping("/lost-reasons-by-pipeline")
    @PreAuthorize("hasRole('CATEGORY_MANAGER')")
    @Operation(summary = "Lost reasons per pipeline", description = "LOST deal reasons grouped by pipeline; each pipeline has its own reason breakdown. Optional category filter.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.LostReasonsByPipelineResponse>> getLostReasonsByPipeline(
            @RequestParam(value = "category", required = false) String category) {
        User currentUser = getCurrentCategoryManager();
        AdminDashboardDtos.LostReasonsByPipelineResponse data =
                categoryManagerDashboardService.getLostReasonsByPipeline(currentUser, category);
        return ResponseEntity.ok(ApiResponse.success("Lost reasons by pipeline fetched", data));
    }

    @GetMapping("/lost-reasons-by-user")
    @PreAuthorize("hasRole('CATEGORY_MANAGER')")
    @Operation(summary = "Lost reasons per user", description = "LOST deal reasons grouped by sales user; each user has their own reason breakdown. Optional category filter.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.LostReasonsByUserResponse>> getLostReasonsByUser(
            @RequestParam(value = "category", required = false) String category) {
        User currentUser = getCurrentCategoryManager();
        AdminDashboardDtos.LostReasonsByUserResponse data =
                categoryManagerDashboardService.getLostReasonsByUser(currentUser, category);
        return ResponseEntity.ok(ApiResponse.success("Lost reasons by user fetched", data));
    }

    @GetMapping("/user-activities-monthly")
    @PreAuthorize("hasRole('CATEGORY_MANAGER')")
    @Operation(summary = "Monthly user activities", description = "Monthly activities per user (total, calls, meetings, durations) for the year, scoped to this Category Manager's hierarchy.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.UserActivityMonthlySummaryResponse>> getUserActivityMonthlySummary(
            @RequestParam("year") Integer year) {
        if (year == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("year is required"));
        }
        User currentUser = getCurrentCategoryManager();
        AdminDashboardDtos.UserActivityMonthlySummaryResponse data =
                categoryManagerDashboardService.getUserActivityMonthlySummary(currentUser, year);
        return ResponseEntity.ok(ApiResponse.success("User activity monthly summary fetched", data));
    }

    @GetMapping("/organization-deals-status")
    @PreAuthorize("hasRole('CATEGORY_MANAGER')")
    @Operation(summary = "Deal status per organization", description = "Deal status per organization (all time + monthly by year), scoped to organizations owned by users in this Category Manager's hierarchy.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.OrganizationDealStatusSummaryResponse>> getOrganizationDealStatusSummary(
            @RequestParam("year") Integer year) {
        if (year == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("year is required"));
        }
        User currentUser = getCurrentCategoryManager();
        AdminDashboardDtos.OrganizationDealStatusSummaryResponse data =
                categoryManagerDashboardService.getOrganizationDealStatusSummary(currentUser, year);
        return ResponseEntity.ok(ApiResponse.success("Organization deal status summary fetched", data));
    }

    @GetMapping("/sales-pipelines")
    @PreAuthorize("hasRole('CATEGORY_MANAGER')")
    @Operation(summary = "Sales users and their pipelines",
            description = "Returns, for this Category Manager's hierarchy, the SALES users and the pipelines "
                    + "they own (via pipeline -> organization -> owner). Useful for building filters like "
                    + "\"Sales → Pipelines\" on the dashboard.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.SalesPipelinesResponse>> getSalesUsersWithPipelines() {
        User currentUser = getCurrentCategoryManager();
        AdminDashboardDtos.SalesPipelinesResponse data =
                categoryManagerDashboardService.getSalesUsersWithPipelines(currentUser);
        return ResponseEntity.ok(ApiResponse.success("Sales users with pipelines fetched", data));
    }

    private User getCurrentCategoryManager() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !(authentication.getPrincipal() instanceof UserDetails)) {
            throw new UnauthorizedException("User not authenticated");
        }
        String email = ((UserDetails) authentication.getPrincipal()).getUsername();
        return userRepository.findByEmail(email)
                .orElseThrow(() -> new UnauthorizedException("Current user not found"));
    }
}
