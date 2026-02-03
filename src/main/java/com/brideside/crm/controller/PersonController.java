package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.CustomFilterDtos;
import com.brideside.crm.dto.MergeRequest;
import com.brideside.crm.dto.PersonDTO;
import com.brideside.crm.dto.PersonDtos;
import com.brideside.crm.dto.PersonSummaryDTO;
import com.brideside.crm.entity.Person;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.UnauthorizedException;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.CustomFilterService;
import com.brideside.crm.service.PersonService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/persons")
@Tag(name = "Persons", description = "API for managing leads/person records")
public class PersonController {

    private final PersonService service;
    private final CustomFilterService customFilterService;
    private final UserRepository userRepository;

    public PersonController(PersonService service, CustomFilterService customFilterService, UserRepository userRepository) {
        this.service = service;
        this.customFilterService = customFilterService;
        this.userRepository = userRepository;
    }

    @Operation(summary = "List persons", description = "Search and filter persons by label, owner, organization, category, source, dealSource, and lead date range. Supports multi-select filtering for categoryId, organizationId, ownerId, and label using comma-separated values (e.g., categoryId=1,2,3&label=BRIDAL_MAKEUP,BRIDAL_PLANNING). Also supports duplicate checking with phone, instagramId, and excludeId parameters.")
    @GetMapping
    public Page<PersonDTO> list(
            @RequestParam(name = "q", required = false) String query,
            @RequestParam(name = "label", required = false) String labelCode,
            @RequestParam(name = "source", required = false) String sourceCode,
            @RequestParam(name = "dealSource", required = false) String dealSourceCode,
            @RequestParam(name = "organizationId", required = false) String organizationId,
            @RequestParam(name = "ownerId", required = false) String ownerId,
            @RequestParam(name = "categoryId", required = false) String categoryId,
            @RequestParam(name = "leadFrom", required = false) @DateTimeFormat(pattern = "dd/MM/yyyy") LocalDate leadFrom,
            @RequestParam(name = "leadTo", required = false) @DateTimeFormat(pattern = "dd/MM/yyyy") LocalDate leadTo,
            @RequestParam(name = "phone", required = false) String phone,
            @RequestParam(name = "instagramId", required = false) String instagramId,
            @RequestParam(name = "excludeId", required = false) Long excludeId,
            @ParameterObject @PageableDefault(size = 25, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable
    ) {
        // If phone or instagramId is provided, this is a duplicate check request
        // Return paginated results with duplicates
        if ((phone != null && !phone.trim().isEmpty()) || (instagramId != null && !instagramId.trim().isEmpty())) {
            List<PersonDTO> duplicates = service.checkDuplicates(phone, instagramId, excludeId);
            // Convert to Page for consistency with API format
            int start = (int) pageable.getOffset();
            int end = Math.min((start + pageable.getPageSize()), duplicates.size());
            List<PersonDTO> pageContent = start < duplicates.size() ? duplicates.subList(start, end) : List.of();
            return new org.springframework.data.domain.PageImpl<>(pageContent, pageable, duplicates.size());
        }
        
        // Otherwise, return normal paginated list
        List<Person.PersonLabel> labels = parseCommaSeparatedLabels(labelCode);
        com.brideside.crm.entity.DealSource source = parseSource(sourceCode);
        com.brideside.crm.entity.DealSource dealSource = parseDealSource(dealSourceCode);
        List<Long> organizationIds = parseCommaSeparatedIds(organizationId);
        List<Long> ownerIds = parseCommaSeparatedIds(ownerId);
        List<Long> categoryIds = parseCommaSeparatedIds(categoryId);
        return service.list(query, labels, organizationIds, ownerIds, categoryIds, source, dealSource, leadFrom, leadTo, pageable);
    }

    @Operation(summary = "Search persons (unrestricted)", 
            description = "Search persons without role-based access restrictions. " +
                    "This endpoint is intended for use cases like 'create deal' where users need to search for any person " +
                    "regardless of their role-based access restrictions. " +
                    "Supports the same filters as /api/persons but does NOT apply role-based organization or pipeline filtering. " +
                    "Still respects soft-delete filtering and all user-provided filters.")
    @GetMapping("/search")
    public Page<PersonDTO> searchUnrestricted(
            @RequestParam(name = "q", required = false) String query,
            @RequestParam(name = "label", required = false) String labelCode,
            @RequestParam(name = "source", required = false) String sourceCode,
            @RequestParam(name = "dealSource", required = false) String dealSourceCode,
            @RequestParam(name = "organizationId", required = false) String organizationId,
            @RequestParam(name = "ownerId", required = false) String ownerId,
            @RequestParam(name = "categoryId", required = false) String categoryId,
            @RequestParam(name = "leadFrom", required = false) @DateTimeFormat(pattern = "dd/MM/yyyy") LocalDate leadFrom,
            @RequestParam(name = "leadTo", required = false) @DateTimeFormat(pattern = "dd/MM/yyyy") LocalDate leadTo,
            @ParameterObject @PageableDefault(size = 50, sort = "name", direction = Sort.Direction.ASC) Pageable pageable
    ) {
        List<Person.PersonLabel> labels = parseCommaSeparatedLabels(labelCode);
        com.brideside.crm.entity.DealSource source = parseSource(sourceCode);
        com.brideside.crm.entity.DealSource dealSource = parseDealSource(dealSourceCode);
        List<Long> organizationIds = parseCommaSeparatedIds(organizationId);
        List<Long> ownerIds = parseCommaSeparatedIds(ownerId);
        List<Long> categoryIds = parseCommaSeparatedIds(categoryId);
        return service.listUnrestricted(query, labels, organizationIds, ownerIds, categoryIds, source, dealSource, leadFrom, leadTo, pageable);
    }

    @Operation(summary = "List persons with details", description = "Returns paginated persons with their associated deals and activities in a single optimized response. " +
            "Uses JOINs to efficiently fetch related data. Supports all the same filters as /api/persons endpoint. " +
            "Default page size is 200 (recommended for infinite scroll). " +
            "Response includes persons array, deals array (all deals for the persons in this page), activities array (all activities for the persons in this page), and pagination metadata.")
    @GetMapping("/with-details")
    public ResponseEntity<PersonDtos.PersonsWithDetailsResponse> listWithDetails(
            @RequestParam(name = "q", required = false) String query,
            @RequestParam(name = "label", required = false) String labelCode,
            @RequestParam(name = "source", required = false) String sourceCode,
            @RequestParam(name = "dealSource", required = false) String dealSourceCode,
            @RequestParam(name = "organizationId", required = false) String organizationId,
            @RequestParam(name = "ownerId", required = false) String ownerId,
            @RequestParam(name = "categoryId", required = false) String categoryId,
            @RequestParam(name = "pipelineId", required = false) String pipelineId,
            @RequestParam(name = "leadFrom", required = false) @DateTimeFormat(pattern = "dd/MM/yyyy") LocalDate leadFrom,
            @RequestParam(name = "leadTo", required = false) @DateTimeFormat(pattern = "dd/MM/yyyy") LocalDate leadTo,
            @ParameterObject @PageableDefault(size = 200, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable
    ) {
        List<Person.PersonLabel> labels = parseCommaSeparatedLabels(labelCode);
        com.brideside.crm.entity.DealSource source = parseSource(sourceCode);
        com.brideside.crm.entity.DealSource dealSource = parseDealSource(dealSourceCode);
        List<Long> organizationIds = parseCommaSeparatedIds(organizationId);
        List<Long> ownerIds = parseCommaSeparatedIds(ownerId);
        List<Long> categoryIds = parseCommaSeparatedIds(categoryId);
        List<Long> pipelineIds = parseCommaSeparatedIds(pipelineId);
        
        PersonDtos.PersonsWithDetailsResponse response = service.listWithDetails(
            query, labels, organizationIds, ownerIds, categoryIds, pipelineIds, source, dealSource, leadFrom, leadTo, pageable
        );
        
        return ResponseEntity.ok(response);
    }

    @Operation(summary = "List label options")
    @GetMapping("/labels")
    public List<PersonDTO.EnumOption> labelOptions() {
        return service.listLabelOptions();
    }

    @Operation(summary = "List category options", description = "Returns list of available categories from the categories table")
    @GetMapping("/categories")
    public ResponseEntity<ApiResponse<List<PersonService.CategoryOption>>> categoryOptions() {
        return ResponseEntity.ok(ApiResponse.success("Categories retrieved successfully", service.listCategoryOptions()));
    }

    @Operation(summary = "List source options", description = "Returns list of available deal sources (same as /api/deals/sources)")
    @GetMapping("/sources")
    public ResponseEntity<ApiResponse<List<SourceOption>>> sourceOptions() {
        List<SourceOption> sources = Arrays.asList(
            new SourceOption("Direct", "Direct"),
            new SourceOption("Divert", "Divert"),
            new SourceOption("Reference", "Reference"),
            new SourceOption("Planner", "Planner"),
            new SourceOption("TBS", "TBS")
        );
        return ResponseEntity.ok(ApiResponse.success("Sources retrieved successfully", sources));
    }

    @Operation(summary = "List sub source options", description = "Returns list of available deal sub sources (same as /api/deals/sub-sources)")
    @GetMapping("/sub-sources")
    public ResponseEntity<ApiResponse<List<SourceOption>>> subSourceOptions() {
        List<SourceOption> subSources = Arrays.asList(
            new SourceOption("Instagram", "Instagram"),
            new SourceOption("Whatsapp", "Whatsapp"),
            new SourceOption("Landing Page", "Landing Page"),
            new SourceOption("Email", "Email")
        );
        return ResponseEntity.ok(ApiResponse.success("Sub sources retrieved successfully", subSources));
    }

    @Operation(summary = "Get persons by deal IDs", description = "Returns all persons associated with the specified deal IDs. " +
            "Useful for fetching only the persons related to currently loaded deals. " +
            "Accepts comma-separated deal IDs (e.g., dealIds=1,2,3) or multiple dealIds parameters.")
    @GetMapping("/by-deals")
    public List<PersonDTO> getByDealIds(
            @RequestParam(name = "dealIds", required = false) String dealIdsParam) {
        List<Long> dealIds = parseCommaSeparatedIds(dealIdsParam);
        if (dealIds.isEmpty()) {
            return List.of();
        }
        return service.getByDealIds(dealIds);
    }

    @Operation(summary = "List eligible owners", description = "Returns active SALES users that can own a person")
    @GetMapping("/owners")
    public List<PersonDTO.OwnerOption> ownerOptions() {
        return service.listOwnerOptions();
    }

    @Operation(summary = "Get person by ID")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "200", description = "Person found")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "404", description = "Person not found")
    @GetMapping("/{id:\\d+}")
    public PersonDTO get(@Parameter(description = "Person ID") @PathVariable Long id) {
        return service.get(id);
    }

    @Operation(summary = "Get person by ID with details", description = "Returns person details along with their associated deals and activities in a single optimized response. " +
            "Uses JOINs to efficiently fetch related data. This endpoint replaces the need to call /api/persons/{id}, /api/deals/person/{personId}, and /api/activities?personId={personId} separately.")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "200", description = "Person found with details")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "404", description = "Person not found")
    @GetMapping("/{id:\\d+}/with-details")
    public ResponseEntity<PersonDtos.PersonWithDetailsResponse> getWithDetails(
            @Parameter(description = "Person ID") @PathVariable Long id) {
        PersonDtos.PersonWithDetailsResponse response = service.getWithDetails(id);
        return ResponseEntity.ok(response);
    }

    @Operation(summary = "Get person summary")
    @GetMapping("/{id:\\d+}/summary")
    public PersonSummaryDTO getSummary(@PathVariable Long id) {
        return service.getSummary(id);
    }

    @Operation(
        summary = "Create person",
        description = "Creates a new person. By default, checks for duplicates by phone/Instagram ID. " +
                      "Use skipDuplicateCheck=true query parameter when you plan to merge immediately after creation."
    )
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "201", description = "Person created successfully")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "400", description = "Duplicate person found (when skipDuplicateCheck=false)")
    @PostMapping
    public ResponseEntity<PersonDTO> create(
            @Valid @RequestBody PersonDTO dto,
            @RequestParam(name = "skipDuplicateCheck", required = false, defaultValue = "false") boolean skipDuplicateCheck) {
        return ResponseEntity.status(201).body(service.create(dto, skipDuplicateCheck));
    }

    @Operation(
        summary = "Update person",
        description = "Updates an existing person. By default, checks for duplicates when phone/Instagram ID changes. " +
                      "Use skipDuplicateCheck=true query parameter when you plan to merge immediately after update."
    )
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "200", description = "Person updated successfully")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "400", description = "Duplicate person found (when skipDuplicateCheck=false)")
    @PutMapping("/{id}")
    public PersonDTO update(
            @Parameter(description = "Person ID") @PathVariable Long id,
            @Valid @RequestBody PersonDTO dto,
            @RequestParam(name = "skipDuplicateCheck", required = false, defaultValue = "false") boolean skipDuplicateCheck) {
        return service.update(id, dto, skipDuplicateCheck);
    }

    @Operation(summary = "Delete person")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "204", description = "Person deleted successfully")
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable Long id) {
        service.delete(id);
        return ResponseEntity.noContent().build();
    }

    @Operation(summary = "Bulk delete persons")
    @DeleteMapping
    public ResponseEntity<Long> bulkDelete(@RequestParam List<Long> ids) {
        return ResponseEntity.ok(service.bulkDelete(ids));
    }

    @Operation(
        summary = "Merge duplicate persons",
        description = "Merges one or more duplicate persons into the target person. " +
                      "All data from duplicate persons is merged into the target person using intelligent conflict resolution. " +
                      "All associated deals and activities are reassigned to the target person. " +
                      "Duplicate persons are soft-deleted after successful merge. " +
                      "The operation is atomic - if any step fails, all changes are rolled back."
    )
    @io.swagger.v3.oas.annotations.responses.ApiResponse(
        responseCode = "200",
        description = "Persons merged successfully"
    )
    @io.swagger.v3.oas.annotations.responses.ApiResponse(
        responseCode = "400",
        description = "Bad request - target person not found, duplicate person not found, self-merge attempt, or merge conflict"
    )
    @PostMapping("/{id}/merge")
    public PersonDTO merge(
            @Parameter(description = "Target person ID (the person to keep as primary)") @PathVariable Long id,
            @RequestBody MergeRequest request) {
        return service.merge(id, request != null ? request.getDuplicateIds() : List.of());
    }

    // DEPRECATED: Use /api/custom-filters/persons instead
    // This endpoint is kept for backward compatibility during frontend migration
    @PostMapping("/custom-filters")
    @Deprecated
    public ResponseEntity<ApiResponse<CustomFilterDtos.FilterResponse>> deprecatedCustomFiltersPost(
            @Valid @RequestBody CustomFilterDtos.SaveFilterRequest request) {
        // Forward to new endpoint
        Long userId = getCurrentUserId();
        try {
            CustomFilterDtos.FilterResponse response = customFilterService.saveFilter(userId, "persons", request);
            return ResponseEntity.ok(ApiResponse.success("Filter saved successfully", response));
        } catch (com.brideside.crm.exception.BadRequestException e) {
            if (e.getMessage() != null && e.getMessage().contains("already exists")) {
                return ResponseEntity.status(HttpStatus.CONFLICT)
                        .body(ApiResponse.error("Filter with this name already exists"));
            }
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(ApiResponse.error(e.getMessage()));
        }
    }

    // DEPRECATED: Use /api/custom-filters/persons instead
    @GetMapping("/custom-filters")
    @Deprecated
    public ResponseEntity<java.util.Map<String, java.util.List<CustomFilterDtos.FilterCondition>>> deprecatedCustomFiltersGet() {
        // Forward to new endpoint
        Long userId = getCurrentUserId();
        java.util.Map<String, java.util.List<CustomFilterDtos.FilterCondition>> filters = customFilterService.getAllFilters(userId, "persons");
        return ResponseEntity.ok(filters);
    }

    // DEPRECATED: Use /api/custom-filters/persons/{name} instead
    @DeleteMapping("/custom-filters/{name}")
    @Deprecated
    public ResponseEntity<ApiResponse<Void>> deprecatedCustomFiltersDelete(
            @PathVariable String name) {
        // Forward to new endpoint
        Long userId = getCurrentUserId();
        try {
            String decodedName = java.net.URLDecoder.decode(name, java.nio.charset.StandardCharsets.UTF_8);
            customFilterService.deleteFilter(userId, "persons", decodedName);
            return ResponseEntity.ok(ApiResponse.success("Filter deleted successfully", null));
        } catch (com.brideside.crm.exception.ResourceNotFoundException e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ApiResponse.error("Filter not found"));
        } catch (IllegalArgumentException e) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(ApiResponse.error("Invalid filter name encoding"));
        }
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

    private Person.PersonLabel parseLabel(String value) {
        if (value == null || value.isBlank()) {
            return null;
        }
        try {
            return Person.PersonLabel.valueOf(value.toUpperCase());
        } catch (IllegalArgumentException ex) {
            throw new BadRequestException("Invalid label value: " + value);
        }
    }

    /**
     * Parses comma-separated label string into a list of PersonLabel enums.
     * Handles single values, multiple values, empty strings, and whitespace.
     * Validates that labels are valid enum values and removes duplicates.
     *
     * @param labels Comma-separated string of label codes (e.g., "BRIDAL_MAKEUP,BRIDAL_PLANNING" or "BRIDAL_MAKEUP")
     * @return List of PersonLabel enums, empty list if input is null or empty
     * @throws BadRequestException if any label is invalid
     */
    private List<Person.PersonLabel> parseCommaSeparatedLabels(String labels) {
        if (labels == null || labels.trim().isEmpty()) {
            return Collections.emptyList();
        }
        try {
            return Arrays.stream(labels.split(","))
                    .map(String::trim)
                    .filter(s -> !s.isEmpty())
                    .map(s -> Person.PersonLabel.valueOf(s.toUpperCase()))
                    .distinct() // Remove duplicates
                    .collect(Collectors.toList());
        } catch (IllegalArgumentException e) {
            throw new BadRequestException("Invalid label value: " + labels + ". Valid values are: " + 
                    Arrays.toString(Person.PersonLabel.values()));
        }
    }

    private com.brideside.crm.entity.DealSource parseSource(String value) {
        if (value == null || value.isBlank()) {
            return null;
        }
        return com.brideside.crm.entity.DealSource.fromString(value);
    }

    private com.brideside.crm.entity.DealSource parseDealSource(String value) {
        if (value == null || value.isBlank()) {
            return null;
        }
        com.brideside.crm.entity.DealSource dealSource = com.brideside.crm.entity.DealSource.fromString(value);
        if (dealSource == null) {
            throw new BadRequestException("Invalid dealSource value: " + value + 
                ". Allowed values: Direct, Divert, Reference, Planner, TBS");
        }
        return dealSource;
    }

    /**
     * Parses comma-separated ID string into a list of Long IDs.
     * Handles single values, multiple values, empty strings, and whitespace.
     * Validates that IDs are positive numbers and removes duplicates.
     *
     * @param ids Comma-separated string of IDs (e.g., "1,2,3" or "1")
     * @return List of Long IDs, empty list if input is null or empty
     * @throws BadRequestException if any ID is invalid or not a positive number
     */
    private List<Long> parseCommaSeparatedIds(String ids) {
        if (ids == null || ids.trim().isEmpty()) {
            return Collections.emptyList();
        }
        try {
            return Arrays.stream(ids.split(","))
                    .map(String::trim)
                    .filter(s -> !s.isEmpty())
                    .map(Long::parseLong)
                    .filter(id -> id > 0) // Ensure positive IDs
                    .distinct() // Remove duplicates
                    .collect(Collectors.toList());
        } catch (NumberFormatException e) {
            throw new BadRequestException("Invalid ID format: " + ids + ". Expected comma-separated numbers (e.g., 1,2,3)");
        }
    }

    public static class SourceOption {
        private String code;
        private String label;

        public SourceOption(String code, String label) {
            this.code = code;
            this.label = label;
        }

        public String getCode() {
            return code;
        }

        public void setCode(String code) {
            this.code = code;
        }

        public String getLabel() {
            return label;
        }

        public void setLabel(String label) {
            this.label = label;
        }
    }
}

