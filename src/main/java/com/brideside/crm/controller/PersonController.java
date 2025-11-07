package com.brideside.crm.controller;

import com.brideside.crm.dto.PersonDTO;
import com.brideside.crm.dto.PersonSummaryDTO;
import com.brideside.crm.dto.MergeRequest;
import com.brideside.crm.service.PersonService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@RestController
@RequestMapping("/api/persons")
@CrossOrigin
@Tag(name = "Persons", description = "API for managing persons, filters, and merge operations")
public class PersonController {
    private final PersonService service;
    public PersonController(PersonService service) { this.service = service; }

    private static final Map<String, List<FilterCondition>> SAVED_FILTERS = new ConcurrentHashMap<>();

    @Operation(summary = "List persons with filters", description = "Get paginated list of persons with optional filters: search query, category, organization, manager, wedding venue/date, and date range")
    @GetMapping
    public Page<PersonDTO> list(
            @RequestParam(name = "q", required = false) String q,
            @RequestParam(name = "category", required = false) String category,
            @RequestParam(name = "organization", required = false) String organization,
            @RequestParam(name = "manager", required = false) String manager,
            @RequestParam(name = "dateFrom", required = false) String dateFrom,
            @RequestParam(name = "dateTo", required = false) String dateTo,
            @RequestParam(name = "weddingVenue", required = false) String weddingVenue,
            @RequestParam(name = "weddingDate", required = false) String weddingDate,
            @ParameterObject @PageableDefault(size = 25, sort = {"createdDate"}) Pageable pageable
    ) {
        return service.list(q, category, organization, manager, dateFrom, dateTo, weddingVenue, weddingDate, pageable);
    }

    @Operation(summary = "Get filter metadata", description = "Get distinct values for categories, organizations, managers, and venues for dropdown filters")
    @GetMapping("/filters")
    public java.util.Map<String, java.util.List<String>> filtersMeta() {
        return service.getFilterMeta();
    }

    public static class FilterCondition {
        public String field;
        public String operator;
        public String value;
    }

    public static class CustomFilterRequest {
        public String name;
        public List<FilterCondition> conditions;
    }

    @Operation(summary = "Save a custom persons filter", description = "Store a named list of filter conditions (in-memory demo)")
    @PostMapping("/custom-filters")
    public ResponseEntity<Void> saveCustomFilter(@RequestBody CustomFilterRequest req) {
        if (req == null || req.name == null || req.name.isBlank()) return ResponseEntity.badRequest().build();
        SAVED_FILTERS.put(req.name, req.conditions != null ? req.conditions : new ArrayList<>());
        return ResponseEntity.noContent().build();
    }

    @Operation(summary = "List custom persons filters", description = "Return all saved filters (in-memory demo)")
    @GetMapping("/custom-filters")
    public Map<String, List<FilterCondition>> listCustomFilters() {
        return SAVED_FILTERS;
    }

    @Operation(summary = "Delete a custom persons filter", description = "Remove a saved filter by name (in-memory demo)")
    @DeleteMapping("/custom-filters/{name}")
    public ResponseEntity<Void> deleteCustomFilter(@PathVariable("name") String name) {
        SAVED_FILTERS.remove(name);
        return ResponseEntity.noContent().build();
    }

    @Operation(summary = "Apply ad-hoc custom filter", description = "Post conditions and get paginated persons without saving")
    @PostMapping("/apply-filter")
    public Page<PersonDTO> applyCustomFilter(@RequestBody CustomFilterRequest req, @ParameterObject @PageableDefault(size = 25, sort = {"createdDate"}) Pageable pageable) {
        String q = null, category = null, organization = null, manager = null, dateFrom = null, dateTo = null, weddingVenue = null, weddingDate = null;
        if (req != null && req.conditions != null) {
            for (FilterCondition c : req.conditions) {
                if (c == null || c.field == null) continue;
                switch (c.field) {
                    case "q": if ("contains".equalsIgnoreCase(c.operator) || c.operator == null) q = c.value; break;
                    case "category": if ("equals".equalsIgnoreCase(c.operator) || c.operator == null) category = c.value; break;
                    case "organization": if ("equals".equalsIgnoreCase(c.operator) || c.operator == null) organization = c.value; break;
                    case "manager": if ("equals".equalsIgnoreCase(c.operator) || c.operator == null) manager = c.value; break;
                    case "dateFrom": dateFrom = c.value; break;
                    case "dateTo": dateTo = c.value; break;
                    case "weddingVenue": if ("equals".equalsIgnoreCase(c.operator) || c.operator == null) weddingVenue = c.value; break;
                    case "weddingDate": if ("equals".equalsIgnoreCase(c.operator) || c.operator == null) weddingDate = c.value; break;
                }
            }
        }
        return service.list(q, category, organization, manager, dateFrom, dateTo, weddingVenue, weddingDate, pageable);
    }

    @Operation(summary = "Get managers by category", description = "Get list of managers for a specific category (for cascading dropdown)")
    @GetMapping("/managers")
    public java.util.List<String> managersByCategory(
            @Parameter(description = "Category to filter managers") @RequestParam(name = "category", required = false) String category) {
        return service.getManagersByCategory(category);
    }

    @Operation(summary = "Get managers by organization", description = "Get list of managers for a specific organization (for cascading dropdown)")
    @GetMapping("/managers-by-organization")
    public java.util.List<String> managersByOrganization(
            @Parameter(description = "Organization to filter managers") @RequestParam(name = "organization", required = false) String organization) {
        return service.getManagersByOrganization(organization);
    }

    @Operation(summary = "Get person by ID", description = "Retrieve a single person by their ID")
    @ApiResponse(responseCode = "200", description = "Person found")
    @ApiResponse(responseCode = "404", description = "Person not found")
    @GetMapping("/{id}")
    public PersonDTO get(@Parameter(description = "Person ID") @PathVariable("id") Long id) { return service.get(id); }

    @Operation(summary = "Get person summary", description = "Get person details with deal count (for detail view pane)")
    @GetMapping("/{id}/summary")
    public PersonSummaryDTO getSummary(@Parameter(description = "Person ID") @PathVariable("id") Long id) {
        return service.getSummary(id);
    }

    @Operation(summary = "List persons by organization", description = "Filter persons by organization")
    @GetMapping("/by-organization")
    public Page<PersonDTO> personsByOrganization(
            @RequestParam(name = "organization", required = false) String organization,
            @RequestParam(name = "page", required = false) Integer page,
            @RequestParam(name = "size", required = false) Integer size,
            @RequestParam(name = "sort", required = false) String sort) {
        Pageable pageable = buildSafePageable(page, size, sort);
        return service.list(null, null, organization, null, null, null, null, null, pageable);
    }

    @Operation(summary = "List persons by manager", description = "Filter persons by manager")
    @GetMapping("/by-manager")
    public Page<PersonDTO> personsByManager(
            @RequestParam(name = "manager", required = false) String manager,
            @RequestParam(name = "page", required = false) Integer page,
            @RequestParam(name = "size", required = false) Integer size,
            @RequestParam(name = "sort", required = false) String sort) {
        Pageable pageable = buildSafePageable(page, size, sort);
        return service.list(null, null, null, manager, null, null, null, null, pageable);
    }

    @Operation(summary = "List persons by category", description = "Filter persons by category")
    @GetMapping("/by-category")
    public Page<PersonDTO> personsByCategory(
            @RequestParam(name = "category", required = false) String category,
            @RequestParam(name = "page", required = false) Integer page,
            @RequestParam(name = "size", required = false) Integer size,
            @RequestParam(name = "sort", required = false) String sort) {
        Pageable pageable = buildSafePageable(page, size, sort);
        return service.list(null, category, null, null, null, null, null, null, pageable);
    }

    private Pageable buildSafePageable(Integer page, Integer size, String sortParam) {
        int p = page != null && page >= 0 ? page : 0;
        int s = size != null && size > 0 ? size : 25;
        if (sortParam == null || sortParam.isBlank() || sortParam.trim().startsWith("[")) {
            return PageRequest.of(p, s, Sort.by("createdAt").descending());
        }
        String trimmed = sortParam.trim();
        String[] parts = trimmed.split(",");
        String property = parts[0].trim();
        Sort.Direction direction = Sort.Direction.DESC;
        if (parts.length > 1) {
            try {
                direction = Sort.Direction.fromString(parts[1].trim());
            } catch (IllegalArgumentException ignored) {
            }
        }
        if (property.isEmpty()) {
            return PageRequest.of(p, s, Sort.by("createdAt").descending());
        }
        return PageRequest.of(p, s, Sort.by(direction, property));
    }

    @Operation(summary = "List persons by created date range", description = "Filter persons by createdDate range dd/MM/yyyy")
    @GetMapping("/by-date")
    public Page<PersonDTO> personsByDate(
            @RequestParam(name = "from", required = false) String from,
            @RequestParam(name = "to", required = false) String to,
            @ParameterObject @PageableDefault(size = 25, sort = {"createdDate"}) Pageable pageable) {
        return service.list(null, null, null, null, from, to, null, null, pageable);
    }

    @Operation(summary = "Create person", description = "Create a new person")
    @ApiResponse(responseCode = "201", description = "Person created successfully")
    @PostMapping
    public ResponseEntity<PersonDTO> create(@Valid @RequestBody PersonDTO dto) {
        return ResponseEntity.status(201).body(service.create(dto));
    }

    @Operation(summary = "Update person", description = "Update an existing person by ID")
    @ApiResponse(responseCode = "200", description = "Person updated successfully")
    @ApiResponse(responseCode = "404", description = "Person not found")
    @PutMapping("/{id}")
    public PersonDTO update(@Parameter(description = "Person ID") @PathVariable("id") Long id, @Valid @RequestBody PersonDTO dto) {
        return service.update(id, dto);
    }

    @Operation(summary = "Delete person", description = "Delete a person by ID")
    @ApiResponse(responseCode = "204", description = "Person deleted successfully")
    @ApiResponse(responseCode = "404", description = "Person not found")
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@Parameter(description = "Person ID") @PathVariable("id") Long id) {
        service.delete(id);
        return ResponseEntity.noContent().build();
    }

    @Operation(summary = "Bulk delete persons", description = "Delete multiple persons by IDs")
    @DeleteMapping
    public ResponseEntity<Long> bulkDelete(@Parameter(description = "List of person IDs to delete") @RequestParam List<Long> ids) {
        return ResponseEntity.ok(service.bulkDelete(ids));
    }

    @Operation(summary = "Merge duplicate persons", description = "Merge duplicate persons into target person. Fills empty fields from duplicates and deletes duplicates")
    @ApiResponse(responseCode = "200", description = "Persons merged successfully")
    @ApiResponse(responseCode = "404", description = "Target person not found")
    @PostMapping("/{id}/merge")
    public PersonDTO merge(
            @Parameter(description = "Target person ID to keep") @PathVariable("id") Long id,
            @RequestBody MergeRequest request) {
        return service.merge(id, request != null ? request.getDuplicateIds() : java.util.Collections.emptyList());
    }
}


