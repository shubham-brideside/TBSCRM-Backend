package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.CustomFilterDtos;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.UnauthorizedException;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.CustomFilterService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.Map;

@RestController
@RequestMapping("/api/custom-filters")
@Tag(name = "Custom Filters", description = "API for managing custom filters for different entities (persons, deals, activities)")
public class CustomFilterController {

    private final CustomFilterService customFilterService;
    private final UserRepository userRepository;

    public CustomFilterController(CustomFilterService customFilterService, UserRepository userRepository) {
        this.customFilterService = customFilterService;
        this.userRepository = userRepository;
    }

    @Operation(
        summary = "Get all custom filters for an entity",
        description = "Retrieve all saved custom filters for the current user for a specific entity type (persons, deals, activities)"
    )
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "200", description = "Filters retrieved successfully")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "400", description = "Invalid entity type")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "401", description = "Unauthorized")
    @GetMapping("/{entityType}")
    public ResponseEntity<Map<String, java.util.List<CustomFilterDtos.FilterCondition>>> getAllCustomFilters(
            @Parameter(description = "Entity type: persons, deals, or activities", example = "persons")
            @PathVariable String entityType) {
        String normalizedType = validateAndNormalizeEntityType(entityType);
        Long userId = getCurrentUserId();
        Map<String, java.util.List<CustomFilterDtos.FilterCondition>> filters = customFilterService.getAllFilters(userId, normalizedType);
        return ResponseEntity.ok(filters);
    }

    @Operation(
        summary = "Save custom filter",
        description = "Save a new custom filter or update an existing one for a specific entity type. Filter names must be unique per user and entity type."
    )
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "200", description = "Filter saved successfully")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "201", description = "Filter created successfully")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "400", description = "Bad Request - validation error")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "401", description = "Unauthorized")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "409", description = "Conflict - duplicate filter name")
    @PostMapping
    public ResponseEntity<ApiResponse<Object>> saveCustomFilterWithoutEntityType() {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                .body(ApiResponse.error("Entity type is required. Use POST /api/custom-filters/{entityType} where entityType is 'persons', 'deals', or 'activities'."));
    }

    @Operation(
        summary = "Save custom filter",
        description = "Save a new custom filter or update an existing one for a specific entity type. Filter names must be unique per user and entity type."
    )
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "200", description = "Filter saved successfully")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "201", description = "Filter created successfully")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "400", description = "Bad Request - validation error")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "401", description = "Unauthorized")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "409", description = "Conflict - duplicate filter name")
    @PostMapping("/{entityType}")
    public ResponseEntity<ApiResponse<CustomFilterDtos.FilterResponse>> saveCustomFilter(
            @Parameter(description = "Entity type: persons, deals, or activities", example = "persons")
            @PathVariable String entityType,
            @Valid @RequestBody CustomFilterDtos.SaveFilterRequest request) {
        String normalizedType = validateAndNormalizeEntityType(entityType);
        Long userId = getCurrentUserId();
        
        try {
            CustomFilterDtos.FilterResponse response = customFilterService.saveFilter(userId, normalizedType, request);
            return ResponseEntity.ok(ApiResponse.success("Filter saved successfully", response));
        } catch (com.brideside.crm.exception.BadRequestException e) {
            // Check if it's a duplicate name error
            if (e.getMessage() != null && e.getMessage().contains("already exists")) {
                return ResponseEntity.status(HttpStatus.CONFLICT)
                        .body(ApiResponse.error("Filter with this name already exists for " + normalizedType));
            }
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(ApiResponse.error(e.getMessage()));
        }
    }

    @Operation(
        summary = "Delete custom filter",
        description = "Delete a custom filter by name for a specific entity type. Filter name should be URL-encoded."
    )
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "200", description = "Filter deleted successfully")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "400", description = "Invalid entity type or filter name encoding")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "401", description = "Unauthorized")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "404", description = "Filter not found")
    @DeleteMapping("/{entityType}/{name}")
    public ResponseEntity<ApiResponse<Void>> deleteCustomFilter(
            @Parameter(description = "Entity type: persons, deals, or activities", example = "persons")
            @PathVariable String entityType,
            @Parameter(description = "Filter name (URL-encoded)")
            @PathVariable String name) {
        String normalizedType = validateAndNormalizeEntityType(entityType);
        Long userId = getCurrentUserId();
        
        try {
            // URL decode the filter name
            String decodedName = URLDecoder.decode(name, StandardCharsets.UTF_8);
            customFilterService.deleteFilter(userId, normalizedType, decodedName);
            return ResponseEntity.ok(ApiResponse.success("Filter deleted successfully", null));
        } catch (com.brideside.crm.exception.ResourceNotFoundException e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ApiResponse.error("Filter not found"));
        } catch (IllegalArgumentException e) {
            // Invalid URL encoding
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(ApiResponse.error("Invalid filter name encoding"));
        }
    }

    /**
     * Validates that the entity type is one of the supported types and returns normalized version
     */
    private String validateAndNormalizeEntityType(String entityType) {
        if (entityType == null || entityType.trim().isEmpty()) {
            throw new com.brideside.crm.exception.BadRequestException("Entity type is required");
        }
        String normalizedType = entityType.toLowerCase().trim();
        if (!normalizedType.equals("persons") && !normalizedType.equals("deals") && !normalizedType.equals("activities")) {
            throw new com.brideside.crm.exception.BadRequestException(
                "Invalid entity type: " + entityType + ". Supported types: persons, deals, activities"
            );
        }
        return normalizedType;
    }

    /**
     * Gets the current authenticated user's ID from the security context
     */
    private Long getCurrentUserId() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !(authentication.getPrincipal() instanceof UserDetails)) {
            throw new UnauthorizedException("User not authenticated");
        }
        
        UserDetails userDetails = (UserDetails) authentication.getPrincipal();
        String email = userDetails.getUsername();
        
        User user = userRepository.findByEmail(email)
                .orElseThrow(() -> new UnauthorizedException("User not found"));
        
        return user.getId();
    }
}
