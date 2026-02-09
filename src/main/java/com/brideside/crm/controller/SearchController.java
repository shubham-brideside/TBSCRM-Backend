package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.SearchDtos;
import com.brideside.crm.service.SearchService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/search")
@Tag(name = "Search", description = "Global search APIs for searching across persons and deals")
public class SearchController {
    
    private final SearchService searchService;
    
    public SearchController(SearchService searchService) {
        this.searchService = searchService;
    }
    
    @GetMapping
    @PreAuthorize("isAuthenticated()")
    @Operation(
        summary = "Global search",
        description = "Search across persons and deals by name, instagram_id, and phone_number. " +
                     "Returns matching persons and deals in a single response. " +
                     "Searches in: person name, instagram ID, phone, email; deal name, venue, phone number, and related person/organization names. " +
                     "Results are filtered based on user role: Admin sees all, Category Manager sees their orgs and those of Sales/Presales under them, " +
                     "Sales sees their orgs and those of Presales under them, Presales sees only their own orgs.",
        security = @SecurityRequirement(name = "Bearer Authentication")
    )
    public ResponseEntity<ApiResponse<SearchDtos.GlobalSearchResponse>> globalSearch(
            @Parameter(description = "Search query - searches in name, instagram_id, and phone_number", required = true)
            @RequestParam(required = true) String q,
            @Parameter(description = "Maximum number of results per entity type (persons and deals). Default: 10")
            @RequestParam(required = false) Integer limit) {
        
        if (q == null || q.trim().isEmpty()) {
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Search query 'q' parameter is required and cannot be empty"));
        }
        
        SearchDtos.GlobalSearchResponse result = searchService.globalSearch(q.trim(), limit);
        
        return ResponseEntity.ok(ApiResponse.success(
            String.format("Found %d persons and %d deals", result.getPersonsCount(), result.getDealsCount()),
            result
        ));
    }
}

