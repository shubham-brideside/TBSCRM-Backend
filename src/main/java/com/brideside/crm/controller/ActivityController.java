package com.brideside.crm.controller;

import com.brideside.crm.dto.ActivityDTO;
import com.brideside.crm.dto.ActivityDtos;
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
@Tag(name = "Activities", description = "APIs for creating and listing activities across all persons (categories: Activity, Call, Meeting scheduler). " +
        "Supporting endpoints for activities page: GET /api/organizations (organization filter), GET /api/users (assigned user dropdown), " +
        "GET /api/deals (deal linking), GET /api/persons (person selection with pagination)")
public class ActivityController {
    private final ActivityService service;
    public ActivityController(ActivityService service) { this.service = service; }

    @Operation(summary = "List activities", description = "Get paginated list of activities with optional filters: personId, date range, assignedUser, category (Activity/Call/Meeting scheduler), status, done")
    @GetMapping
    public Page<ActivityDTO> list(
            @RequestParam(name = "personId", required = false) Long personId,
            @RequestParam(name = "dateFrom", required = false) String dateFrom,
            @RequestParam(name = "dateTo", required = false) String dateTo,
            @RequestParam(name = "assignedUser", required = false) String assignedUser,
            @RequestParam(name = "category", required = false) String category,
            @RequestParam(name = "status", required = false) String status,
            @RequestParam(name = "done", required = false) Boolean done,
            @ParameterObject @PageableDefault(size = 25, sort = {"date"}) Pageable pageable
    ) {
        return service.list(personId, dateFrom, dateTo, assignedUser, category, status, done, pageable);
    }

    @Operation(summary = "Create activity", description = "Create a new activity. 'subject' is required (trimmed non-empty string). 'category' defaults to ACTIVITY if not provided. " +
            "Fields excluded from create: 'done' (use POST /api/activities/{id}/done to toggle). Legacy fields 'scheduleBy' and 'callType' are no longer exposed in the API. " +
            "All other fields are optional. Supports all column fields for Activity, Call, and Meeting scheduler tabs.")
    @PostMapping
    public ResponseEntity<ActivityDTO> create(@Valid @RequestBody ActivityDTO dto) {
        return ResponseEntity.status(201).body(service.create(dto));
    }

    @Operation(summary = "Update activity", description = "Update an existing activity. Accepts same payload shape as POST /api/activities. Always performs partial update - only provided fields are updated. Used for editing single activity, bulk inline edits, and duration/note tweaks.")
    @PutMapping("/{id}")
    public ResponseEntity<ActivityDTO> update(@PathVariable("id") Long id, @RequestBody ActivityDTO dto) {
        return ResponseEntity.ok(service.update(id, dto));
    }

    @Operation(summary = "Delete activity", description = "Delete an activity by ID. Used for multi-select bulk delete from the table.")
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable("id") Long id) {
        service.delete(id);
        return ResponseEntity.noContent().build();
    }

    @Operation(summary = "Toggle done state", description = "Toggle the done flag of an activity. Used by checkbox/toggle that marks an activity as completed or re-opens it.")
    @PostMapping("/{id}/done")
    public ResponseEntity<ActivityDTO> markDone(
            @PathVariable("id") Long id,
            @RequestParam("value") boolean value
    ) {
        return ResponseEntity.ok(service.markDone(id, value));
    }

    @Operation(summary = "List activity categories", description = "Returns activity category options for filter dropdowns and Activity modal's 'Activity type' select. Each item includes only 'code' and 'label'.")
    @GetMapping("/categories")
    public ResponseEntity<List<ActivityDtos.CategoryOption>> categories() {
        return ResponseEntity.ok(ActivityDtos.allCategoryOptions());
    }
}


