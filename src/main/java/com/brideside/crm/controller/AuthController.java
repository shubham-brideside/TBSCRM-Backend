package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.ForgotPasswordRequest;
import com.brideside.crm.dto.LoginRequest;
import com.brideside.crm.dto.LoginResponse;
import com.brideside.crm.dto.PageAccessDtos;
import com.brideside.crm.dto.ResetPasswordRequest;
import com.brideside.crm.service.AuthService;
import com.brideside.crm.service.PageAccessService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/auth")
@Tag(name = "Authentication", description = "Authentication APIs")
public class AuthController {

    @Autowired
    private AuthService authService;

    @Autowired
    private PageAccessService pageAccessService;

    @PostMapping("/login")
    @Operation(summary = "Login user", description = "Authenticate user and return JWT token")
    public ResponseEntity<ApiResponse<LoginResponse>> login(@Valid @RequestBody LoginRequest request) {
        LoginResponse response = authService.login(request);
        return ResponseEntity.ok(ApiResponse.success("Login successful", response));
    }

    @PostMapping("/forgot-password")
    @Operation(summary = "Forgot password", description = "Request password reset. Sends a password reset email to the user.")
    public ResponseEntity<ApiResponse<Void>> forgotPassword(@Valid @RequestBody ForgotPasswordRequest request) {
        authService.forgotPassword(request);
        return ResponseEntity.ok(ApiResponse.success("Password reset email sent. Please check your inbox."));
    }

    @PostMapping("/reset-password")
    @Operation(summary = "Reset password", description = "Reset password using the token received via email.")
    public ResponseEntity<ApiResponse<Void>> resetPassword(@Valid @RequestBody ResetPasswordRequest request) {
        authService.resetPassword(request);
        return ResponseEntity.ok(ApiResponse.success("Password reset successfully. You can now login with your new password."));
    }

    @GetMapping("/page-access")
    @PreAuthorize("isAuthenticated()")
    @Operation(
        summary = "Get current user page access", 
        description = "Retrieve page access permissions for the currently authenticated user. This endpoint should be called on login or when checking permissions."
    )
    @SecurityRequirement(name = "Bearer Authentication")
    public ResponseEntity<ApiResponse<PageAccessDtos.PageAccessResponse>> getCurrentUserPageAccess() {
        String currentUserEmail = getCurrentUserEmail();
        PageAccessDtos.PageAccessResponse response = pageAccessService.getCurrentUserPageAccess(currentUserEmail);
        return ResponseEntity.ok(ApiResponse.success("Page access retrieved successfully", response));
    }

    private String getCurrentUserEmail() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.getPrincipal() instanceof org.springframework.security.core.userdetails.UserDetails) {
            return ((org.springframework.security.core.userdetails.UserDetails) authentication.getPrincipal()).getUsername();
        }
        throw new com.brideside.crm.exception.UnauthorizedException("User not authenticated");
    }
}

