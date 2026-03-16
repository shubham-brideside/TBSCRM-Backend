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
@RequestMapping("/api/presales/dashboard")
@Tag(name = "Presales Dashboard", description = "Same filters as sales dashboard: dateFrom, dateTo, pipelineId on every route.")
public class PresalesDashboardController {

    private final SalesDashboardService salesDashboardService;
    private final UserRepository userRepository;

    public PresalesDashboardController(SalesDashboardService salesDashboardService,
                                       UserRepository userRepository) {
        this.salesDashboardService = salesDashboardService;
        this.userRepository = userRepository;
    }

    private String validateFilters(LocalDate dateFrom, LocalDate dateTo, Long pipelineId, User user) {
        if ((dateFrom == null) != (dateTo == null)) {
            return "dateFrom and dateTo must both be set or both omitted";
        }
        if (pipelineId != null && !salesDashboardService.isPipelineOwnedBySalesUser(pipelineId, user)) {
            return "pipelineId must be a pipeline belonging to your Sales manager's organization";
        }
        return null;
    }

    @GetMapping("/summary")
    @PreAuthorize("hasRole('PRESALES')")
    @Operation(summary = "Dashboard summary")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.SummaryResponse>> getSummary(
            @RequestParam(value = "dateFrom", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(value = "dateTo", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        User user = getCurrentUser();
        String err = validateFilters(dateFrom, dateTo, pipelineId, user);
        if (err != null) {
            return ResponseEntity.badRequest().body(ApiResponse.error(err));
        }
        return ResponseEntity.ok(ApiResponse.success("Presales dashboard summary fetched",
                salesDashboardService.getDashboardSummary(user, dateFrom, dateTo, pipelineId)));
    }

    @GetMapping("/deals-status-monthly")
    @PreAuthorize("hasRole('PRESALES')")
    @Operation(summary = "Monthly deal status")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.DealStatusMonthlyResponse>> getDealStatusMonthly(
            @RequestParam("year") Integer year,
            @RequestParam(value = "dateFrom", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(value = "dateTo", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        if (year == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("year is required"));
        }
        User user = getCurrentUser();
        String err = validateFilters(dateFrom, dateTo, pipelineId, user);
        if (err != null) {
            return ResponseEntity.badRequest().body(ApiResponse.error(err));
        }
        return ResponseEntity.ok(ApiResponse.success("Deal status monthly fetched",
                salesDashboardService.getDealStatusMonthly(user, year, dateFrom, dateTo, pipelineId)));
    }

    @GetMapping("/revenue")
    @PreAuthorize("hasRole('PRESALES')")
    @Operation(summary = "Revenue by pipeline")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.RevenueResponse>> getRevenue(
            @RequestParam(value = "dateFrom", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(value = "dateTo", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        User user = getCurrentUser();
        String err = validateFilters(dateFrom, dateTo, pipelineId, user);
        if (err != null) {
            return ResponseEntity.badRequest().body(ApiResponse.error(err));
        }
        return ResponseEntity.ok(ApiResponse.success("Revenue fetched",
                salesDashboardService.getRevenue(user, dateFrom, dateTo, pipelineId)));
    }

    @GetMapping("/lost-reasons")
    @PreAuthorize("hasRole('PRESALES')")
    @Operation(summary = "Lost deal reasons")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.LostReasonsResponse>> getLostReasons(
            @RequestParam(value = "category", required = false) String category,
            @RequestParam(value = "dateFrom", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(value = "dateTo", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        User user = getCurrentUser();
        String err = validateFilters(dateFrom, dateTo, pipelineId, user);
        if (err != null) {
            return ResponseEntity.badRequest().body(ApiResponse.error(err));
        }
        return ResponseEntity.ok(ApiResponse.success("Lost reasons fetched",
                salesDashboardService.getLostReasons(user, category, dateFrom, dateTo, pipelineId)));
    }

    @GetMapping("/lost-reasons-by-organization")
    @PreAuthorize("hasRole('PRESALES')")
    @Operation(summary = "Lost reasons per organization")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.LostReasonsByOrganizationResponse>> getLostReasonsByOrganization(
            @RequestParam(value = "category", required = false) String category,
            @RequestParam(value = "dateFrom", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(value = "dateTo", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        User user = getCurrentUser();
        String err = validateFilters(dateFrom, dateTo, pipelineId, user);
        if (err != null) {
            return ResponseEntity.badRequest().body(ApiResponse.error(err));
        }
        return ResponseEntity.ok(ApiResponse.success("Lost reasons by organization fetched",
                salesDashboardService.getLostReasonsByOrganization(user, category, dateFrom, dateTo, pipelineId)));
    }

    @GetMapping("/lost-reasons-by-pipeline")
    @PreAuthorize("hasRole('PRESALES')")
    @Operation(summary = "Lost reasons per pipeline")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.LostReasonsByPipelineResponse>> getLostReasonsByPipeline(
            @RequestParam(value = "category", required = false) String category,
            @RequestParam(value = "dateFrom", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(value = "dateTo", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        User user = getCurrentUser();
        String err = validateFilters(dateFrom, dateTo, pipelineId, user);
        if (err != null) {
            return ResponseEntity.badRequest().body(ApiResponse.error(err));
        }
        return ResponseEntity.ok(ApiResponse.success("Lost reasons by pipeline fetched",
                salesDashboardService.getLostReasonsByPipeline(user, category, dateFrom, dateTo, pipelineId)));
    }

    @GetMapping("/lost-deals-by-stage-per-organization")
    @PreAuthorize("hasRole('PRESALES')")
    @Operation(summary = "Lost deals by stage per organization")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.LostDealsByStagePerOrganizationResponse>> getLostDealsByStagePerOrganization(
            @RequestParam(value = "category", required = false) String category,
            @RequestParam(value = "dateFrom", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(value = "dateTo", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        User user = getCurrentUser();
        String err = validateFilters(dateFrom, dateTo, pipelineId, user);
        if (err != null) {
            return ResponseEntity.badRequest().body(ApiResponse.error(err));
        }
        return ResponseEntity.ok(ApiResponse.success("Lost deals by stage per organization fetched",
                salesDashboardService.getLostDealsByStagePerOrganization(user, category, dateFrom, dateTo, pipelineId)));
    }

    @GetMapping("/lost-deals-by-stage")
    @PreAuthorize("hasRole('PRESALES')")
    @Operation(summary = "Lost deals by stage")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.LostDealsByStageResponse>> getLostDealsByStage(
            @RequestParam(value = "category", required = false) String category,
            @RequestParam(value = "dateFrom", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(value = "dateTo", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        User user = getCurrentUser();
        String err = validateFilters(dateFrom, dateTo, pipelineId, user);
        if (err != null) {
            return ResponseEntity.badRequest().body(ApiResponse.error(err));
        }
        return ResponseEntity.ok(ApiResponse.success("Lost deals by stage fetched",
                salesDashboardService.getLostDealsByStage(user, category, dateFrom, dateTo, pipelineId)));
    }

    @GetMapping("/activities-monthly")
    @PreAuthorize("hasRole('PRESALES')")
    @Operation(summary = "Monthly activity summary")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.ActivityMonthlyResponse>> getActivityMonthly(
            @RequestParam("year") Integer year,
            @RequestParam(value = "dateFrom", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(value = "dateTo", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        if (year == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("year is required"));
        }
        User user = getCurrentUser();
        String err = validateFilters(dateFrom, dateTo, pipelineId, user);
        if (err != null) {
            return ResponseEntity.badRequest().body(ApiResponse.error(err));
        }
        return ResponseEntity.ok(ApiResponse.success("Activity monthly summary fetched",
                salesDashboardService.getActivityMonthly(user, year, dateFrom, dateTo, pipelineId)));
    }

    @GetMapping("/pipeline-performance")
    @PreAuthorize("hasRole('PRESALES')")
    @Operation(summary = "Pipeline performance")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.PipelinePerformanceResponse>> getPipelinePerformance(
            @RequestParam(value = "dateFrom", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(value = "dateTo", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        User user = getCurrentUser();
        String err = validateFilters(dateFrom, dateTo, pipelineId, user);
        if (err != null) {
            return ResponseEntity.badRequest().body(ApiResponse.error(err));
        }
        return ResponseEntity.ok(ApiResponse.success("Pipeline performance fetched",
                salesDashboardService.getPipelinePerformance(user, dateFrom, dateTo, pipelineId)));
    }

    @GetMapping("/target-vs-achievement")
    @PreAuthorize("hasRole('PRESALES')")
    @Operation(summary = "Target vs achievement")
    public ResponseEntity<ApiResponse<SalesDashboardDtos.TargetVsAchievementResponse>> getTargetVsAchievement(
            @RequestParam("year") Integer year,
            @RequestParam(value = "dateFrom", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(value = "dateTo", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        if (year == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("year is required"));
        }
        User user = getCurrentUser();
        String err = validateFilters(dateFrom, dateTo, pipelineId, user);
        if (err != null) {
            return ResponseEntity.badRequest().body(ApiResponse.error(err));
        }
        return ResponseEntity.ok(ApiResponse.success("Target vs achievement fetched",
                salesDashboardService.getTargetVsAchievement(user, year, dateFrom, dateTo, pipelineId)));
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
