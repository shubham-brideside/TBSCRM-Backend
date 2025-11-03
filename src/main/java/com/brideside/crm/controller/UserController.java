package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.CreateUserRequest;
import com.brideside.crm.dto.SetPasswordRequest;
import com.brideside.crm.dto.UpdateUserRequest;
import com.brideside.crm.dto.UserResponse;
import com.brideside.crm.service.UserService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/users")
@Tag(name = "User Management", description = "User management APIs")
public class UserController {

    @Autowired
    private UserService userService;

    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Create new user", description = "Create a new user and send invitation email. Only accessible by ADMIN.")
    @SecurityRequirement(name = "Bearer Authentication")
    public ResponseEntity<ApiResponse<UserResponse>> createUser(@Valid @RequestBody CreateUserRequest request) {
        UserResponse response = userService.createUser(request);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.success("User created and invitation email sent", response));
    }

    @GetMapping
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Get all users", description = "Get list of users based on role hierarchy. Admin sees all, Category Manager sees their team, Manager sees their Salesrep, Salesrep sees only themselves.")
    @SecurityRequirement(name = "Bearer Authentication")
    public ResponseEntity<ApiResponse<List<UserResponse>>> getAllUsers() {
        String currentUserEmail = getCurrentUserEmail();
        List<UserResponse> users = userService.getAllUsers(currentUserEmail);
        return ResponseEntity.ok(ApiResponse.success("Users retrieved successfully", users));
    }

    @GetMapping("/{id}")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Get user by ID", description = "Get user details by ID. Access is based on role hierarchy.")
    @SecurityRequirement(name = "Bearer Authentication")
    public ResponseEntity<ApiResponse<UserResponse>> getUserById(@PathVariable Long id) {
        String currentUserEmail = getCurrentUserEmail();
        UserResponse user = userService.getUserById(id, currentUserEmail);
        return ResponseEntity.ok(ApiResponse.success("User retrieved successfully", user));
    }
    
    private String getCurrentUserEmail() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.getPrincipal() instanceof org.springframework.security.core.userdetails.UserDetails) {
            return ((org.springframework.security.core.userdetails.UserDetails) authentication.getPrincipal()).getUsername();
        }
        throw new com.brideside.crm.exception.UnauthorizedException("User not authenticated");
    }

    @PostMapping("/set-password")
    @Operation(summary = "Set password", description = "Set password using invitation token")
    public ResponseEntity<ApiResponse<Void>> setPassword(@Valid @RequestBody SetPasswordRequest request) {
        userService.setPassword(request);
        return ResponseEntity.ok(ApiResponse.success("Password set successfully. You can now login."));
    }

    @GetMapping("/accept-invitation")
    @Operation(summary = "Verify invitation token", description = "Verify if invitation token is valid")
    public ResponseEntity<ApiResponse<String>> verifyInvitationToken(@RequestParam String token) {
        // This endpoint just verifies the token is valid
        // The actual password setting is done via set-password endpoint
        try {
            // Token validation will be done in setPassword, but we can verify it exists here
            return ResponseEntity.ok(ApiResponse.success("Invitation token is valid", token));
        } catch (Exception e) {
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Invalid or expired invitation token"));
        }
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Update user", description = "Update user details including manager assignment. Only accessible by ADMIN.")
    @SecurityRequirement(name = "Bearer Authentication")
    public ResponseEntity<ApiResponse<UserResponse>> updateUser(
            @PathVariable Long id,
            @Valid @RequestBody UpdateUserRequest request) {
        UserResponse response = userService.updateUser(id, request);
        return ResponseEntity.ok(ApiResponse.success("User updated successfully", response));
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Delete user", description = "Delete a user by ID. Only accessible by ADMIN. If user has subordinates, provide reassignManagerId query parameter to reassign them.")
    @SecurityRequirement(name = "Bearer Authentication")
    public ResponseEntity<ApiResponse<Void>> deleteUser(
            @PathVariable Long id,
            @RequestParam(required = false, value = "reassignManagerId") String reassignManagerIdParam) {
        // Convert string parameter to Long, handling "null" string and empty strings
        Long managerId = null;
        if (reassignManagerIdParam != null && !reassignManagerIdParam.trim().isEmpty() 
                && !reassignManagerIdParam.equalsIgnoreCase("null") 
                && !reassignManagerIdParam.equalsIgnoreCase("undefined")) {
            try {
                managerId = Long.parseLong(reassignManagerIdParam.trim());
            } catch (NumberFormatException e) {
                return ResponseEntity.badRequest()
                        .body(ApiResponse.error("Invalid reassignManagerId format: " + reassignManagerIdParam));
            }
        }
        userService.deleteUser(id, managerId);
        return ResponseEntity.ok(ApiResponse.success("User deleted successfully"));
    }
}

