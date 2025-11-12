package com.brideside.crm.controller;

import com.brideside.crm.dto.ActivityDTO;
import com.brideside.crm.dto.ActivityDtos;
import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.service.ActivityService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/activities")
@CrossOrigin
@Tag(name = "Activities", description = "APIs for creating and listing activities across all persons (categories: Activity, Call, Meeting scheduler)")
public class ActivityController {
    private final ActivityService service;
    public ActivityController(ActivityService service) { this.service = service; }

    @Operation(summary = "List activities", description = "Get paginated list of activities with optional filters: personId, date range, assignedUser, category (Activity/Call/Meeting scheduler), status, callType, done")
    @GetMapping
    public Page<ActivityDTO> list(
            @RequestParam(name = "personId", required = false) Long personId,
            @RequestParam(name = "dateFrom", required = false) String dateFrom,
            @RequestParam(name = "dateTo", required = false) String dateTo,
            @RequestParam(name = "assignedUser", required = false) String assignedUser,
            @RequestParam(name = "category", required = false) String category,
            @RequestParam(name = "status", required = false) String status,
            @RequestParam(name = "callType", required = false) String callType,
            @RequestParam(name = "done", required = false) Boolean done,
            @ParameterObject @PageableDefault(size = 25, sort = {"date"}) Pageable pageable
    ) {
        return service.list(personId, dateFrom, dateTo, assignedUser, category, status, callType, done, pageable);
    }

    @Operation(summary = "Create activity", description = "Create a new activity. For testing, only 'subject' is required. 'personId', 'dealId' and 'dateTime' are optional right now and will be enforced later.")
    @PostMapping
    public ResponseEntity<ActivityDTO> create(@Valid @RequestBody ActivityDTO dto) {
        return ResponseEntity.status(201).body(service.create(dto));
    }

    @Operation(summary = "List activity categories", description = "Returns dropdown options for activity categories")
    @GetMapping("/categories")
    public ResponseEntity<ApiResponse<List<ActivityDtos.CategoryOption>>> categories() {
        return ResponseEntity.ok(ApiResponse.success("Activity categories fetched", ActivityDtos.allCategoryOptions()));
    }

    @Operation(summary = "Update activity", description = "Update an existing activity")
    @PutMapping("/{id}")
    public ActivityDTO update(@PathVariable("id") Long id, @RequestBody ActivityDTO dto) {
        return service.update(id, dto);
    }

    @Operation(summary = "Mark activity done/undone", description = "Toggle done flag of an activity")
    @PostMapping("/{id}/done")
    public ActivityDTO markDone(@PathVariable("id") Long id, @RequestParam("value") boolean value) {
        return service.markDone(id, value);
    }

    @Operation(summary = "Delete activity", description = "Delete an activity by ID")
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable("id") Long id) {
        service.delete(id);
        return ResponseEntity.noContent().build();
    }
}


