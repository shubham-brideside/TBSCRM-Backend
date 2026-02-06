package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.AdminDashboardDtos;
import com.brideside.crm.dto.CreateAdminRequest;
import com.brideside.crm.dto.UserResponse;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.service.AdminDashboardService;
import com.brideside.crm.service.AuthService;
import com.brideside.crm.service.EmailService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

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
                    + "Deals are attributed via pipeline -> organization -> owner (SALES role).")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.MonthlyWonDealsBySalesUserResponse>> getWonDealsBySalesUserMonthly(
            @RequestParam("year") Integer year) {
        if (year == null) {
            throw new BadRequestException("year is required");
        }
        AdminDashboardDtos.MonthlyWonDealsBySalesUserResponse data =
                adminDashboardService.getWonDealsBySalesUserMonthly(year);
        return ResponseEntity.ok(
                ApiResponse.success("Monthly won deals grouped by sales user fetched", data)
        );
    }

    @GetMapping("/dashboard/lost-deals-by-sales-user/monthly")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Monthly lost deals grouped by SALES users",
            description = "Returns, for a given year, monthly counts and total values of LOST deals grouped by sales users. "
                    + "Deals are attributed via pipeline -> organization -> owner (SALES role).")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.MonthlyLostDealsBySalesUserResponse>> getLostDealsBySalesUserMonthly(
            @RequestParam("year") Integer year) {
        if (year == null) {
            throw new BadRequestException("year is required");
        }
        AdminDashboardDtos.MonthlyLostDealsBySalesUserResponse data =
                adminDashboardService.getLostDealsBySalesUserMonthly(year);
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

    @GetMapping("/dashboard/lost-reasons")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
            summary = "Lost deal reasons summary",
            description = "Returns all-time LOST deal reasons with count and percentage for donut charts "
                    + "on the admin dashboard.")
    public ResponseEntity<ApiResponse<AdminDashboardDtos.LostReasonSummaryResponse>> getLostReasonSummary() {
        AdminDashboardDtos.LostReasonSummaryResponse data =
                adminDashboardService.getLostReasonSummary();
        return ResponseEntity.ok(
                ApiResponse.success("Lost reason summary fetched", data)
        );
    }
}

