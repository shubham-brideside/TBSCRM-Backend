package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.PageAccessDtos;
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

import java.util.List;

@RestController
@RequestMapping("/api/users")
@Tag(name = "Page Access Management", description = "APIs for managing user page access permissions")
public class PageAccessController {

    @Autowired
    private PageAccessService pageAccessService;

    @GetMapping("/{userId}/page-access")
    @PreAuthorize("isAuthenticated()")
    @Operation(
        summary = "Get user page access", 
        description = "Retrieve all page access permissions for a specific user. Admin can view anyone's access, users can view their own."
    )
    @SecurityRequirement(name = "Bearer Authentication")
    public ResponseEntity<ApiResponse<PageAccessDtos.PageAccessResponse>> getUserPageAccess(
            @PathVariable Long userId) {
        String currentUserEmail = getCurrentUserEmail();
        PageAccessDtos.PageAccessResponse response = pageAccessService.getUserPageAccess(userId, currentUserEmail);
        return ResponseEntity.ok(ApiResponse.success("Page access retrieved successfully", response));
    }

    @PutMapping("/{userId}/page-access")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
        summary = "Update user page access (bulk)", 
        description = "Update page access permissions for a specific user. This is a bulk update operation that can update multiple pages at once. Only accessible by ADMIN."
    )
    @SecurityRequirement(name = "Bearer Authentication")
    public ResponseEntity<ApiResponse<PageAccessDtos.PageAccessResponse>> updateUserPageAccess(
            @PathVariable Long userId,
            @Valid @RequestBody PageAccessDtos.UpdatePageAccessRequest request) {
        String currentUserEmail = getCurrentUserEmail();
        PageAccessDtos.PageAccessResponse response = pageAccessService.updateUserPageAccess(userId, request, currentUserEmail);
        return ResponseEntity.ok(ApiResponse.success("Page access updated successfully", response));
    }

    @PatchMapping("/{userId}/page-access/{pageName}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
        summary = "Update single page access", 
        description = "Update access for a single page for a specific user. Only accessible by ADMIN."
    )
    @SecurityRequirement(name = "Bearer Authentication")
    public ResponseEntity<ApiResponse<PageAccessDtos.SinglePageAccessResponse>> updateSinglePageAccess(
            @PathVariable Long userId,
            @PathVariable String pageName,
            @Valid @RequestBody PageAccessDtos.UpdateSinglePageAccessRequest request) {
        String currentUserEmail = getCurrentUserEmail();
        PageAccessDtos.SinglePageAccessResponse response = pageAccessService.updateSinglePageAccess(
                userId, pageName, request, currentUserEmail);
        return ResponseEntity.ok(ApiResponse.success("Page access updated successfully", response));
    }

    @GetMapping("/page-access-summary")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(
        summary = "Get all users with page access summary", 
        description = "Get a summary of page access for all users. Useful for the admin page access management interface. Only accessible by ADMIN."
    )
    @SecurityRequirement(name = "Bearer Authentication")
    public ResponseEntity<ApiResponse<List<PageAccessDtos.PageAccessSummaryResponse>>> getAllUsersPageAccessSummary() {
        String currentUserEmail = getCurrentUserEmail();
        List<PageAccessDtos.PageAccessSummaryResponse> response = pageAccessService.getAllUsersPageAccessSummary(currentUserEmail);
        return ResponseEntity.ok(ApiResponse.success("Page access summary retrieved successfully", response));
    }

    private String getCurrentUserEmail() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.getPrincipal() instanceof org.springframework.security.core.userdetails.UserDetails) {
            return ((org.springframework.security.core.userdetails.UserDetails) authentication.getPrincipal()).getUsername();
        }
        throw new com.brideside.crm.exception.UnauthorizedException("User not authenticated");
    }
}

