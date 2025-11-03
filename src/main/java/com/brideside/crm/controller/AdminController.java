package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.CreateAdminRequest;
import com.brideside.crm.dto.UserResponse;
import com.brideside.crm.service.AuthService;
import com.brideside.crm.service.EmailService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/admin")
@Tag(name = "Admin", description = "Admin APIs")
public class AdminController {

    @Autowired
    private AuthService authService;

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
}

