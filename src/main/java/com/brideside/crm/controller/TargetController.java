package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.TargetDtos;
import com.brideside.crm.entity.TargetCategory;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.service.TargetService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/api/targets")
@Tag(name = "Targets", description = " Target APIs ")
public class TargetController {

    private final TargetService targetService;

    public TargetController(TargetService targetService) {
        this.targetService = targetService;
    }

    @GetMapping("/dashboard")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Target dashboard data", description = "Returns per-category target vs achieved metrics plus won deal list.")
    public ApiResponse<TargetDtos.DashboardResponse> dashboard(
            @RequestParam(value = "category", required = false) String category,
            @RequestParam(value = "timePreset", required = false) String timePreset,
            @RequestParam(value = "month", required = false) Integer month,
            @RequestParam(value = "year", required = false) Integer year,
            @RequestParam(value = "fromMonth", required = false) Integer fromMonth,
            @RequestParam(value = "fromYear", required = false) Integer fromYear,
            @RequestParam(value = "toMonth", required = false) Integer toMonth,
            @RequestParam(value = "toYear", required = false) Integer toYear
    ) {
        TargetDtos.DashboardFilter filter = new TargetDtos.DashboardFilter();
        filter.category = parseCategory(category);
        filter.preset = TargetDtos.TargetTimePreset.fromValue(timePreset);
        filter.month = month;
        filter.year = year;
        filter.fromMonth = fromMonth;
        filter.fromYear = fromYear;
        filter.toMonth = toMonth;
        filter.toYear = toYear;

        return ApiResponse.success("Target dashboard fetched", targetService.dashboard(filter));
    }

    @GetMapping("/category-breakdown")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Category monthly breakdown", description = "Returns month-wise aggregates and per-user rows for a category across longer periods (quarter/half-year/year).")
    public ApiResponse<TargetDtos.CategoryMonthlyBreakdownResponse> categoryBreakdown(
            @RequestParam(value = "category") String category,
            @RequestParam(value = "timePreset") String timePreset,
            @RequestParam(value = "month", required = false) Integer month,
            @RequestParam(value = "year", required = false) Integer year,
            @RequestParam(value = "fromMonth", required = false) Integer fromMonth,
            @RequestParam(value = "fromYear", required = false) Integer fromYear,
            @RequestParam(value = "toMonth", required = false) Integer toMonth,
            @RequestParam(value = "toYear", required = false) Integer toYear
    ) {
        TargetCategory parsedCategory = parseCategory(category);
        if (parsedCategory == null) {
            throw new BadRequestException("category is required");
        }

        TargetDtos.TargetTimePreset preset = TargetDtos.TargetTimePreset.fromValue(timePreset);
        if (preset == null) {
            throw new BadRequestException("timePreset must be THIS_YEAR, THIS_QUARTER, HALF_YEAR, or CUSTOM_RANGE");
        }

        TargetDtos.DashboardFilter filter = new TargetDtos.DashboardFilter();
        filter.category = parsedCategory;
        filter.preset = preset;
        filter.month = month;
        filter.year = year;
        filter.fromMonth = fromMonth;
        filter.fromYear = fromYear;
        filter.toMonth = toMonth;
        filter.toYear = toYear;

        return ApiResponse.success("Category breakdown fetched",
                targetService.categoryMonthlyBreakdown(filter));
    }

    @GetMapping("/filters")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Target filter metadata", description = "Lists category codes, preset options and min selectable year.")
    public ApiResponse<TargetDtos.FiltersResponse> filters() {
        return ApiResponse.success("Target filters fetched", targetService.filters());
    }

    @GetMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Get target", description = "Fetch a target for editing.")
    public ApiResponse<TargetDtos.TargetResponse> get(@PathVariable("id") Long id) {
        return ApiResponse.success("Target fetched", targetService.get(id));
    }

    @GetMapping({"", "/"})
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "List targets for month", description = "Admin helper endpoint to list/edit targets for a specific month and category.")
    public ApiResponse<List<TargetDtos.TargetResponse>> list(
            @RequestParam(value = "month", required = false) Integer month,
            @RequestParam(value = "year", required = false) Integer year,
            @RequestParam(value = "category", required = false) String category
    ) {
        TargetDtos.TargetListFilter filter = new TargetDtos.TargetListFilter();
        filter.month = month;
        filter.year = year;
        filter.category = category;
        return ApiResponse.success("Targets fetched", targetService.list(filter));
    }

    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Create target", description = "Create a monthly target for a sales user and category.")
    public ApiResponse<TargetDtos.TargetResponse> create(@RequestBody TargetDtos.TargetUpsertRequest request) {
        return ApiResponse.success("Target created", targetService.create(request));
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Update target", description = "Update an existing monthly target record.")
    public ApiResponse<TargetDtos.TargetResponse> update(
            @PathVariable("id") Long id,
            @RequestBody TargetDtos.TargetUpsertRequest request) {
        return ApiResponse.success("Target updated", targetService.update(id, request));
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Delete target", description = "Remove a target entry.")
    public ApiResponse<Void> delete(@PathVariable("id") Long id) {
        targetService.delete(id);
        return ApiResponse.success("Target deleted");
    }

    @GetMapping("/sales-users")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Get sales users with organizations", description = "Returns list of all SALES users with their associated organizations.")
    public ApiResponse<List<TargetDtos.SalesUserWithOrganizations>> getSalesUsersWithOrganizations() {
        return ApiResponse.success("Sales users with organizations fetched", 
                targetService.getSalesUsersWithOrganizations());
    }

    @GetMapping("/sales-users/{userId}/organizations")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Get organizations for sales user", description = "Returns all organizations assigned to a specific SALES user, grouped by month.")
    public ApiResponse<TargetDtos.SalesUserOrganizationsResponse> getSalesUserOrganizations(
            @PathVariable("userId") Long userId) {
        return ApiResponse.success("Sales user organizations fetched", 
                targetService.getSalesUserOrganizations(userId));
    }
    
    @GetMapping("/users/{userId}/detail")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "User monthly target breakdown", description = "Returns per-month target vs achieved statistics plus deal source splits for a sales user.")
    public ApiResponse<TargetDtos.TargetUserMonthlyDetailResponse> getUserMonthlyDetail(
            @PathVariable("userId") Long userId,
            @RequestParam("year") Integer year) {
        if (year == null) {
            throw new BadRequestException("year is required");
        }
        return ApiResponse.success("User monthly detail fetched",
                targetService.getUserMonthlyDetail(userId, year));
    }

    private TargetCategory parseCategory(String value) {
        if (value == null || value.isBlank()) {
            return null;
        }
        TargetCategory category = TargetCategory.fromValue(value);
        if (category == null) {
            throw new BadRequestException("Unknown category: " + value);
        }
        return category;
    }
}

