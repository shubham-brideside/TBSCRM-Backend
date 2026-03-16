package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.AdminDashboardDtos;
import com.brideside.crm.dto.CreateAdminRequest;
import com.brideside.crm.dto.UserResponse;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.service.AdminDashboardService;
import com.brideside.crm.service.AuthService;
import com.brideside.crm.service.DailyOpsReportService;
import com.brideside.crm.service.EmailService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;

@RestController
@RequestMapping("/api/admin")
@Tag(name = "Admin", description = "Admin APIs")
public class AdminController {

    @Autowired
    private AuthService authService;

    @Autowired
    private AdminDashboardService adminDashboardService;

    @Autowired
    private EmailService emailService;

    @Autowired
    private DailyOpsReportService dailyOpsReportService;

    @PostMapping("/create-admin")
    @Operation(summary = "Create first admin user", description = "Create the first admin user. This endpoint is only accessible before any admin exists.")
    public ResponseEntity<ApiResponse<UserResponse>> createAdmin(@Valid @RequestBody CreateAdminRequest request) {
        UserResponse response = authService.createAdminUser(request);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.success("Admin user created successfully", response));
    }

    @PostMapping("/activate-admin")
    @Operation(summary = "Activate admin user", description = "Activate an existing admin user account. Useful if admin was created before activation logic was added.")
    public ResponseEntity<ApiResponse<String>> activateAdmin(@RequestParam String email) {
        authService.activateAdminUser(email);
        return ResponseEntity.ok(ApiResponse.success("Admin user activated successfully. You can now login."));
    }

    @PostMapping("/test-email")
    @Operation(summary = "Test email sending", description = "Send a test email to verify email configuration. Useful for testing MailerSend setup.")
    public ResponseEntity<ApiResponse<String>> testEmail(@RequestParam String toEmail) {
        try {
            String subject = "Brideside CRM - Test Email";
            String message = "This is a test email from Brideside CRM.\n\n" +
                    "If you received this email, your email configuration is working correctly!\n\n" +
                    "Sent at: " + java.time.LocalDateTime.now();
            
            emailService.sendTestEmail(toEmail, subject, message);
            return ResponseEntity.ok(ApiResponse.success("Test email sent successfully to: " + toEmail));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(ApiResponse.error("Failed to send test email: " + e.getMessage()));
        }
    }

    @PostMapping("/reports/daily/test-send")
    @Operation(
            summary = "Send daily ops report (test)",
            description = "Sends the daily ops report email for the previous day to the provided email. Intended for testing.")
    public ResponseEntity<ApiResponse<String>> testSendDailyOpsReport(
            @RequestParam String toEmail,
            @RequestParam(value = "reportDate", required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate reportDate
    ) {
        try {
            if (reportDate == null) {
                dailyOpsReportService.sendDailyReportTo(toEmail);
            } else {
                dailyOpsReportService.sendDailyReportTo(toEmail, reportDate);
            }
            return ResponseEntity.ok(ApiResponse.success("Daily report email sent successfully to: " + toEmail));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(ApiResponse.error("Failed to send daily report email: " + e.getMessage()));
        }
    }

    @GetMapping("/dashboard/won-deals-by-sales-user")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Won deals grouped by SALES users",
            description = "Returns aggregated counts and total values of WON deals grouped by sales users. "
                    + "Deals are attributed via pipeline -> organization -> owner (SALES role).")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.WonDealsBySalesUserResponse>> getWonDealsBySalesUser() {
        AdminDashboardDtos.WonDealsBySalesUserResponse data =
                adminDashboardService.getWonDealsBySalesUser();
        return ResponseEntity.ok(
                ApiResponse.success("Won deals grouped by sales user fetched", data)
        );
    }

    @GetMapping("/dashboard/lost-deals-by-sales-user")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Lost deals grouped by SALES users",
            description = "Returns aggregated counts and total values of LOST deals grouped by sales users. "
                    + "Deals are attributed via pipeline -> organization -> owner (SALES role).")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.LostDealsBySalesUserResponse>> getLostDealsBySalesUser() {
        AdminDashboardDtos.LostDealsBySalesUserResponse data =
                adminDashboardService.getLostDealsBySalesUser();
        return ResponseEntity.ok(
                ApiResponse.success("Lost deals grouped by sales user fetched", data)
        );
    }

    @GetMapping("/dashboard/won-deals-by-sales-user/monthly")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Monthly won deals grouped by SALES users",
            description = "Returns, for a given year, monthly counts and total values of WON deals grouped by sales users. "
                    + "Deals are attributed via pipeline -> organization -> owner (SALES role). "
                    + "Optionally filter by organization category and/or date range within the given year.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.MonthlyWonDealsBySalesUserResponse>> getWonDealsBySalesUserMonthly(
            @RequestParam("year") Integer year,
            @RequestParam(value = "category", required = false) String category,
            @RequestParam(value = "dateFrom", required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(value = "dateTo", required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo) {
        if (year == null) {
            throw new BadRequestException("year is required");
        }
        AdminDashboardDtos.MonthlyWonDealsBySalesUserResponse data =
                adminDashboardService.getWonDealsBySalesUserMonthly(year, category, dateFrom, dateTo);
        return ResponseEntity.ok(
                ApiResponse.success("Monthly won deals grouped by sales user fetched", data)
        );
    }

    @GetMapping("/dashboard/lost-deals-by-sales-user/monthly")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Monthly lost deals grouped by SALES users",
            description = "Returns, for a given year, monthly counts and total values of LOST deals grouped by sales users. "
                    + "Deals are attributed via pipeline -> organization -> owner (SALES role). "
                    + "Optionally filter by organization category and/or date range within the given year.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.MonthlyLostDealsBySalesUserResponse>> getLostDealsBySalesUserMonthly(
            @RequestParam("year") Integer year,
            @RequestParam(value = "category", required = false) String category,
            @RequestParam(value = "dateFrom", required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam(value = "dateTo", required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo) {
        if (year == null) {
            throw new BadRequestException("year is required");
        }
        AdminDashboardDtos.MonthlyLostDealsBySalesUserResponse data =
                adminDashboardService.getLostDealsBySalesUserMonthly(year, category, dateFrom, dateTo);
        return ResponseEntity.ok(
                ApiResponse.success("Monthly lost deals grouped by sales user fetched", data)
        );
    }

    @GetMapping("/dashboard/deals-status-monthly")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Monthly deal status summary (WON / LOST / IN_PROGRESS)",
            description = "Returns, for a given year, monthly counts and total values of deals broken down by status "
                    + "(WON, LOST, IN_PROGRESS) for the admin dashboard.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.DealStatusMonthlySummaryResponse>> getDealStatusMonthlySummary(
            @RequestParam("year") Integer year) {
        if (year == null) {
            throw new BadRequestException("year is required");
        }
        AdminDashboardDtos.DealStatusMonthlySummaryResponse data =
                adminDashboardService.getDealStatusMonthlySummary(year);
        return ResponseEntity.ok(
                ApiResponse.success("Monthly deal status summary fetched", data)
        );
    }

    @GetMapping("/dashboard/user-activities-monthly")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Monthly activities per user (including call counts and duration)",
            description = "Returns, for a given year, per-user monthly activity counts plus call count and total call "
                    + "duration in minutes for the admin dashboard.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.UserActivityMonthlySummaryResponse>> getUserActivityMonthlySummary(
            @RequestParam("year") Integer year) {
        if (year == null) {
            throw new BadRequestException("year is required");
        }
        AdminDashboardDtos.UserActivityMonthlySummaryResponse data =
                adminDashboardService.getUserActivityMonthlySummary(year);
        return ResponseEntity.ok(
                ApiResponse.success("Monthly user activity summary fetched", data)
        );
    }

    @GetMapping("/dashboard/organization-deals-status")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Deal status per organization (all time + monthly by year)",
            description = "Returns, for each organization, all-time WON/LOST/IN_PROGRESS counts and values plus a "
                    + "per-month breakdown for the specified year.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.OrganizationDealStatusSummaryResponse>> getOrganizationDealStatusSummary(
            @RequestParam("year") Integer year) {
        if (year == null) {
            throw new BadRequestException("year is required");
        }
        AdminDashboardDtos.OrganizationDealStatusSummaryResponse data =
                adminDashboardService.getOrganizationDealStatusSummary(year);
        return ResponseEntity.ok(
                ApiResponse.success("Organization deal status summary fetched", data)
        );
    }

    @GetMapping("/dashboard/revenue")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Revenue summary (WON deals) by category, user, and pipeline",
            description = "Returns, for a given date range, the count and total value of WON, non-deleted deals "
                    + "aggregated per organization category, per SALES user (via pipeline -> organization -> owner), "
                    + "and per pipeline.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.RevenueSummaryResponse>> getRevenueSummary(
            @RequestParam("dateFrom")
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam("dateTo")
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo) {
        if (dateFrom == null || dateTo == null) {
            throw new BadRequestException("dateFrom and dateTo are required");
        }
        AdminDashboardDtos.RevenueSummaryResponse data =
                adminDashboardService.getRevenueSummary(dateFrom, dateTo);
        return ResponseEntity.ok(
                ApiResponse.success("Revenue summary fetched", data)
        );
    }

    @GetMapping("/dashboard/commission-by-pipeline")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Commission summary per pipeline (WON deals)",
            description = "Returns, for a given date range, the count, total value, and total commission of WON, "
                    + "non-deleted deals aggregated per pipeline. Optionally filter by organization category.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.CommissionByPipelineResponse>> getCommissionByPipeline(
            @RequestParam(value = "category", required = false) String category,
            @RequestParam("dateFrom")
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateFrom,
            @RequestParam("dateTo")
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate dateTo
    ) {
        if (dateFrom == null || dateTo == null) {
            throw new BadRequestException("dateFrom and dateTo are required");
        }
        AdminDashboardDtos.CommissionByPipelineResponse data =
                adminDashboardService.getCommissionByPipeline(category, dateFrom, dateTo);
        return ResponseEntity.ok(
                ApiResponse.success("Commission by pipeline fetched", data)
        );
    }

    @GetMapping("/dashboard/lost-reasons")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Lost deal reasons summary",
            description = "Returns all-time LOST deal reasons with count and percentage for donut charts "
                    + "on the admin dashboard. "
                    + "Optionally filter by organization category, SALES user and/or pipeline within all-time LOST deals.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.LostReasonSummaryResponse>> getLostReasonSummary(
            @RequestParam(value = "category", required = false) String category,
            @RequestParam(value = "userId", required = false) Long userId,
            @RequestParam(value = "pipelineId", required = false) Long pipelineId) {
        AdminDashboardDtos.LostReasonSummaryResponse data =
                adminDashboardService.getLostReasonSummary(category, userId, pipelineId);
        return ResponseEntity.ok(
                ApiResponse.success("Lost reason summary fetched", data)
        );
    }

    @GetMapping("/dashboard/deal-divert-report")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Deal divert report",
            description = "Returns all WON diverted deals with details: diverted-from pipeline, diverted-to pipeline, "
                    + "and owner of the deal (current pipeline's organization owner). "
                    + "Returns all-time totals + optional monthly breakdown if year parameter provided.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.DealDivertReportResponse>> getDealDivertReport(
            @RequestParam(value = "year", required = false) Integer year) {
        AdminDashboardDtos.DealDivertReportResponse data = adminDashboardService.getDealDivertReport(year);
        return ResponseEntity.ok(
                ApiResponse.success("Deal divert report fetched", data)
        );
    }

    @GetMapping("/dashboard/deal-divert-report/all")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Deal divert report (all diverted deals)",
            description = "Returns all diverted deals (any status) with details: diverted-from pipeline, diverted-to pipeline, "
                    + "and owner of the deal (current pipeline's organization owner). "
                    + "Returns all-time totals + optional monthly breakdown if year parameter provided.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.DealDivertReportResponse>> getAllDivertedDealsReport(
            @RequestParam(value = "year", required = false) Integer year) {
        AdminDashboardDtos.DealDivertReportResponse data = adminDashboardService.getAllDivertedDealsReport(year);
        return ResponseEntity.ok(
                ApiResponse.success("All diverted deals report fetched", data)
        );
    }

    @GetMapping("/dashboard/divert-count-by-user")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Who diverts deals most to least",
            description = "Returns a ranking of users by the number of deals they have diverted (created as diverted deals), "
                    + "ordered from most to least. Uses non-deleted deals where isDiverted=true or dealSource=Divert. "
                    + "Optional query param 'year': when provided, response includes month-wise breakdown for that year.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.DivertCountByUserResponse>> getDivertCountByUser(
            @RequestParam(value = "year", required = false) Integer year) {
        AdminDashboardDtos.DivertCountByUserResponse data = adminDashboardService.getDivertCountByUser(year);
        return ResponseEntity.ok(
                ApiResponse.success("Divert count by user fetched", data)
        );
    }

    @GetMapping("/dashboard/instagram-deals-by-organization")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Instagram deals grouped by organization",
            description = "Returns all deals with subsource=Instagram grouped by organization. "
                    + "Returns all-time totals + optional monthly breakdown if year parameter provided.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.InstagramDealsByOrganizationResponse>> getInstagramDealsByOrganization(
            @RequestParam(value = "year", required = false) Integer year) {
        AdminDashboardDtos.InstagramDealsByOrganizationResponse data = adminDashboardService.getInstagramDealsByOrganization(year);
        return ResponseEntity.ok(
                ApiResponse.success("Instagram deals by organization fetched", data)
        );
    }
}

