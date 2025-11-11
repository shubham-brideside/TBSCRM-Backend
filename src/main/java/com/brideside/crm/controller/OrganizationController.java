package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.OrganizationDtos;
import com.brideside.crm.service.OrganizationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
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
}


