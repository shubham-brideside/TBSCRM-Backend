package com.brideside.crm.controller;

import com.brideside.crm.dto.ActivityDTO;
import com.brideside.crm.dto.ActivityDtos;
import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.service.ActivityScopeService;
import com.brideside.crm.service.ActivityService;
import com.brideside.crm.service.AzureBlobStorageService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

@RestController
@RequestMapping("/api/activities")
@CrossOrigin
@Tag(name = "Activities", description = "APIs for creating and listing activities across all persons (categories: Activity, Call, Meeting scheduler). " +
        "Supporting endpoints for activities page: GET /api/organizations (organization filter), GET /api/users (assigned user dropdown), " +
        "GET /api/deals (deal linking), GET /api/persons (person selection with pagination)")
public class ActivityController {
    private final ActivityService service;
    private final ActivityScopeService scopeService;
    
    @Autowired(required = false)
    private AzureBlobStorageService azureBlobStorageService;
    
    public ActivityController(ActivityService service, ActivityScopeService scopeService) {
        this.service = service;
        this.scopeService = scopeService;
    }

    @Operation(summary = "Call summary", description = "Get call counts respecting all filters.")
    @GetMapping("/summary/call")
    public ActivityDtos.Summary callSummary(
            @RequestParam(name = "personId", required = false) Long personId,
            @RequestParam(name = "dateFrom", required = false) String dateFrom,
            @RequestParam(name = "dateTo", required = false) String dateTo,
            @RequestParam(name = "assignedUser", required = false) String assignedUser,
            @RequestParam(name = "organizationId", required = false) List<Long> organizationIds,
            @RequestParam(name = "assignedUserId", required = false) List<Long> assignedUserIds,
            @RequestParam(name = "category", required = false) String category,
            @RequestParam(name = "done", required = false) Boolean done,
            @RequestParam(name = "serviceCategory", required = false) String serviceCategory,
            @RequestParam(name = "organizationCategory", required = false) String organizationCategory) {
        return service.summaryCall(personId, dateFrom, dateTo, assignedUser,
                organizationIds, assignedUserIds, category, done, serviceCategory, organizationCategory);
    }

    @Operation(summary = "Meeting summary", description = "Get meeting counts respecting all filters.")
    @GetMapping("/summary/meeting")
    public ActivityDtos.Summary meetingSummary(
            @RequestParam(name = "personId", required = false) Long personId,
            @RequestParam(name = "dateFrom", required = false) String dateFrom,
            @RequestParam(name = "dateTo", required = false) String dateTo,
            @RequestParam(name = "assignedUser", required = false) String assignedUser,
            @RequestParam(name = "organizationId", required = false) List<Long> organizationIds,
            @RequestParam(name = "assignedUserId", required = false) List<Long> assignedUserIds,
            @RequestParam(name = "category", required = false) String category,
            @RequestParam(name = "done", required = false) Boolean done,
            @RequestParam(name = "serviceCategory", required = false) String serviceCategory,
            @RequestParam(name = "organizationCategory", required = false) String organizationCategory) {
        return service.summaryMeeting(personId, dateFrom, dateTo, assignedUser,
                organizationIds, assignedUserIds, category, done, serviceCategory, organizationCategory);
    }

    @Operation(summary = "List activities", description = "Get paginated list of activities with optional filters: personId, date range, assignedUser, category (Activity/Call/Meeting scheduler), status, done. " +
            "Also supports role-based scoping using organizationId and assignedUserId for SALES, PRESALES and CATEGORY_MANAGER users. " +
            "Supports infinite scroll with default size=20 and sort=dueDate,desc. Multiple organizationId and assignedUserId values can be provided as arrays.")
    @GetMapping
    public Page<ActivityDTO> list(
            @RequestParam(name = "personId", required = false) Long personId,
            @RequestParam(name = "dateFrom", required = false) String dateFrom,
            @RequestParam(name = "dateTo", required = false) String dateTo,
            @RequestParam(name = "serviceCategory", required = false) String serviceCategory,
            @RequestParam(name = "organizationCategory", required = false) String organizationCategory,
            @RequestParam(name = "assignedUser", required = false) String assignedUser,
            @RequestParam(name = "organizationId", required = false) List<Long> organizationIds,
            @RequestParam(name = "assignedUserId", required = false) List<Long> assignedUserIds,
            @RequestParam(name = "category", required = false) String category,
            @RequestParam(name = "status", required = false) String status,
            @RequestParam(name = "done", required = false) Boolean done,
            @RequestParam(name = "dealId", required = false) Long dealId,
            @ParameterObject @PageableDefault(size = 25, sort = {"dueDate"}, direction = org.springframework.data.domain.Sort.Direction.DESC) Pageable pageable
    ) {
        return service.list(personId, dateFrom, dateTo, assignedUser, organizationIds, assignedUserIds,
                category, status, done, serviceCategory, organizationCategory, dealId, pageable);
    }

    @Operation(summary = "Activities summary", description = "Return counts for dashboard cards (total, pending, completed, assign call, meeting scheduled). " +
            "Uses the same role-based scoping and filters as the list endpoint so cards always match the table.")
    @GetMapping("/summary")
    public ResponseEntity<ActivityDtos.Summary> summary(
            @RequestParam(name = "personId", required = false) Long personId,
            @RequestParam(name = "dateFrom", required = false) String dateFrom,
            @RequestParam(name = "dateTo", required = false) String dateTo,
            @RequestParam(name = "serviceCategory", required = false) String serviceCategory,
            @RequestParam(name = "organizationCategory", required = false) String organizationCategory,
            @RequestParam(name = "assignedUser", required = false) String assignedUser,
            @RequestParam(name = "organizationId", required = false) List<Long> organizationIds,
            @RequestParam(name = "assignedUserId", required = false) List<Long> assignedUserIds,
            @RequestParam(name = "category", required = false) String category,
            @RequestParam(name = "status", required = false) String status,
            @RequestParam(name = "done", required = false) Boolean done
    ) {
        ActivityDtos.Summary s = service.summary(personId, dateFrom, dateTo, assignedUser,
                organizationIds, assignedUserIds, category, status, done, serviceCategory, organizationCategory);
        return ResponseEntity.ok(s);
    }

    @Operation(summary = "List scoped filters", description = "Returns organization and user filter options scoped to the current user's role.")
    @GetMapping("/filters")
    public ResponseEntity<ApiResponse<ActivityDtos.FilterOptions>> filters() {
        ActivityScopeService.FilterContext ctx = scopeService.resolveFilterContext();
        ActivityDtos.FilterOptions data = ActivityDtos.buildFilterOptions(ctx);
        return ResponseEntity.ok(ApiResponse.success("Activity filters fetched", data));
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

    @Operation(summary = "Toggle done state", description = "Toggle the done flag of an activity. Used by checkbox/toggle that marks an activity as completed or re-opens it. " +
            "For Call activities, optionally accepts duration_minutes parameter to save call duration when marking as done.")
    @PostMapping("/{id}/done")
    public ResponseEntity<ActivityDTO> markDone(
            @PathVariable("id") Long id,
            @RequestParam("value") boolean value,
            @RequestParam(name = "duration_minutes", required = false) Integer durationMinutes
    ) {
        return ResponseEntity.ok(service.markDone(id, value, durationMinutes));
    }

    @Operation(summary = "List activity categories", description = "Returns activity category options for filter dropdowns and Activity modal's 'Activity type' select. Each item includes only 'code' and 'label'.")
    @GetMapping("/categories")
    public ResponseEntity<List<ActivityDtos.CategoryOption>> categories() {
        return ResponseEntity.ok(ActivityDtos.allCategoryOptions());
    }

    @Operation(
        summary = "Upload activity screenshot", 
        description = "Upload a screenshot image for an activity. The image will be stored in Azure Blob Storage and the URL will be automatically saved to the activity's attachmentUrl field. " +
                     "Only image files (PNG, JPEG, etc.) are accepted. Maximum file size is 10MB."
    )
    @ApiResponses(value = {
        @io.swagger.v3.oas.annotations.responses.ApiResponse(
            responseCode = "200", 
            description = "Screenshot uploaded successfully",
            content = @Content(schema = @Schema(implementation = ApiResponse.class))
        ),
        @io.swagger.v3.oas.annotations.responses.ApiResponse(
            responseCode = "400", 
            description = "Invalid file type or file too large",
            content = @Content(schema = @Schema(implementation = ApiResponse.class))
        ),
        @io.swagger.v3.oas.annotations.responses.ApiResponse(
            responseCode = "404", 
            description = "Activity not found",
            content = @Content(schema = @Schema(implementation = ApiResponse.class))
        ),
        @io.swagger.v3.oas.annotations.responses.ApiResponse(
            responseCode = "503", 
            description = "Azure Blob Storage not configured",
            content = @Content(schema = @Schema(implementation = ApiResponse.class))
        ),
        @io.swagger.v3.oas.annotations.responses.ApiResponse(
            responseCode = "500", 
            description = "Upload failed",
            content = @Content(schema = @Schema(implementation = ApiResponse.class))
        )
    })
    @PostMapping(value = "/{activityId}/upload-screenshot", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<ApiResponse<String>> uploadScreenshot(
            @Parameter(description = "ID of the activity to attach the screenshot to", required = true, example = "1")
            @PathVariable("activityId") Long activityId,
            @Parameter(description = "Image file to upload (PNG, JPEG, etc.). Maximum size: 10MB", required = true)
            @RequestParam("file") MultipartFile file) {
        
        if (azureBlobStorageService == null) {
            return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE)
                    .body(ApiResponse.error("Azure Blob Storage is not configured. Please set AZURE_STORAGE_BLOB_CONNECTION_STRING environment variable."));
        }
        
        // Validate file
        if (file == null || file.isEmpty()) {
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("File is required"));
        }
        
        // Validate file type (only images)
        String contentType = file.getContentType();
        if (contentType == null || !contentType.startsWith("image/")) {
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("Only image files are allowed. Received content type: " + contentType));
        }
        
        // Validate file size (max 10MB)
        long maxSize = 10 * 1024 * 1024; // 10MB
        if (file.getSize() > maxSize) {
            return ResponseEntity.badRequest()
                    .body(ApiResponse.error("File size exceeds maximum allowed size of 10MB"));
        }
        
        try {
            // Upload to Azure Blob Storage
            String blobUrl = azureBlobStorageService.uploadImage(
                    file.getInputStream(),
                    file.getOriginalFilename(),
                    contentType
            );
            
            // Update activity with the attachment URL
            ActivityDTO activityDTO = new ActivityDTO();
            activityDTO.setAttachmentUrl(blobUrl);
            service.update(activityId, activityDTO);
            
            return ResponseEntity.ok(ApiResponse.success("Screenshot uploaded successfully", blobUrl));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(ApiResponse.error("Failed to upload screenshot: " + e.getMessage()));
        }
    }
}


