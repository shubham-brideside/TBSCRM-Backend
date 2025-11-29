package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.OrganizationDtos;
import com.brideside.crm.service.OrganizationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/organizations")
@Tag(name = "Organizations", description = "Organization management APIs")
public class OrganizationController {

    private final OrganizationService organizationService;

    public OrganizationController(OrganizationService organizationService) {
        this.organizationService = organizationService;
    }

    @PostMapping
    @Operation(summary = "Create organization")
    public ResponseEntity<ApiResponse<OrganizationDtos.OrganizationResponse>> create(
            @Valid @RequestBody OrganizationDtos.OrganizationRequest request) {
        OrganizationDtos.OrganizationResponse response = organizationService.create(request);
        return ResponseEntity.status(201).body(ApiResponse.success("Organization created", response));
    }

    @GetMapping("/categories")
    @Operation(summary = "List organization categories")
    public ResponseEntity<ApiResponse<List<OrganizationDtos.CategoryOption>>> categories() {
        return ResponseEntity.ok(ApiResponse.success("Organization categories fetched", organizationService.listCategoryOptions()));
    }

    @GetMapping("/owners")
    @Operation(summary = "List eligible organization owners")
    public ResponseEntity<ApiResponse<List<OrganizationDtos.OwnerOption>>> ownerOptions() {
        return ResponseEntity.ok(ApiResponse.success("Organization owners fetched", organizationService.listOwnerOptions()));
    }

    @GetMapping
    @Operation(summary = "List organizations")
    public ResponseEntity<ApiResponse<List<OrganizationDtos.OrganizationResponse>>> list() {
        return ResponseEntity.ok(ApiResponse.success("Organizations fetched", organizationService.list()));
    }

    @GetMapping("/accessible-for-current-user")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "List organizations accessible to current user", 
               description = "Returns organizations based on role hierarchy. Admin sees all, Category Manager sees their orgs and those of their Sales/Presales, Sales/Presales see their own orgs.")
    @SecurityRequirement(name = "Bearer Authentication")
    public ResponseEntity<ApiResponse<List<OrganizationDtos.OrganizationResponse>>> listAccessibleForCurrentUser() {
        String currentUserEmail = getCurrentUserEmail();
        List<OrganizationDtos.OrganizationResponse> organizations = organizationService.listAccessibleForCurrentUser(currentUserEmail);
        return ResponseEntity.ok(ApiResponse.success("Accessible organizations fetched", organizations));
    }

    @GetMapping("/{id}")
    @Operation(summary = "Get organization by id")
    public ResponseEntity<ApiResponse<OrganizationDtos.OrganizationResponse>> get(@PathVariable("id") Long id) {
        return ResponseEntity.ok(ApiResponse.success("Organization fetched", organizationService.get(id)));
    }

    @PutMapping("/{id}")
    @Operation(summary = "Update organization")
    public ResponseEntity<ApiResponse<OrganizationDtos.OrganizationResponse>> update(
            @PathVariable("id") Long id,
            @Valid @RequestBody OrganizationDtos.OrganizationRequest request) {
        return ResponseEntity.ok(ApiResponse.success("Organization updated", organizationService.update(id, request)));
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "Delete organization")
    public ResponseEntity<ApiResponse<Void>> delete(@PathVariable("id") Long id) {
        organizationService.delete(id);
        return ResponseEntity.ok(ApiResponse.success("Organization deleted"));
    }

    private String getCurrentUserEmail() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.getPrincipal() instanceof org.springframework.security.core.userdetails.UserDetails) {
            return ((org.springframework.security.core.userdetails.UserDetails) authentication.getPrincipal()).getUsername();
        }
        throw new com.brideside.crm.exception.UnauthorizedException("User not authenticated");
    }
}


