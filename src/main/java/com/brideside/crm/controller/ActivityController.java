package com.brideside.crm.controller;

import com.brideside.crm.dto.ActivityDTO;
import com.brideside.crm.service.ActivityService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/activities")
@CrossOrigin
@Tag(name = "Activities", description = "APIs for creating and listing activities across all persons (categories: Activity, Call, Meeting scheduler)")
public class ActivityController {
    private static final Logger logger = LoggerFactory.getLogger(ActivityController.class);
    private final ActivityService service;
    public ActivityController(ActivityService service) { this.service = service; }

    @Operation(summary = "List activities", description = "Get paginated list of activities with optional filters. If personId is not provided, returns ALL activities (including those with and without personId). If personId is provided, returns only activities for that person. Activities created from person pages will appear in the main list when no personId filter is applied.")
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
            @RequestParam(name = "filter", required = false) String filter,
            @ParameterObject @PageableDefault(size = 25, sort = {"date"}) Pageable pageable
    ) {
        logger.debug("Listing activities - PersonId filter: {}, Category: {}, Status: {}, Filter: {}", personId, category, status, filter);
        return service.list(personId, dateFrom, dateTo, assignedUser, category, status, callType, done, filter, pageable);
    }

    @Operation(summary = "Create activity", description = "Create a new activity. Only 'subject' is required. 'personId' and 'dealId' are optional and will default to null. When creating from a person page, include personId to link the activity. Activities with personId will appear in the main activity list when no personId filter is applied.")
    @PostMapping
    public ResponseEntity<ActivityDTO> create(@Valid @RequestBody ActivityDTO dto) {
        logger.debug("Received activity creation request - Subject: {}, Category: {}, Status: {}, CallType: {}, Organization: {}, PersonId: {}, DealId: {}, Deal: {}", 
                dto.getSubject(), dto.getCategory(), dto.getStatus(), dto.getCallType(), dto.getOrganization(), dto.getPersonId(), dto.getDealId(), dto.getDeal());
        try {
            ActivityDTO created = service.create(dto);
            logger.debug("Activity created successfully with ID: {}, PersonId: {}, DealId: {}, Deal: {}", created.getId(), created.getPersonId(), created.getDealId(), created.getDeal());
            return ResponseEntity.status(201).body(created);
        } catch (Exception e) {
            logger.error("Error creating activity: {}", e.getMessage(), e);
            throw e;
        }
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


