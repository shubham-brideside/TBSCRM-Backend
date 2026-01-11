package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.MergeRequest;
import com.brideside.crm.dto.PersonDTO;
import com.brideside.crm.dto.PersonSummaryDTO;
import com.brideside.crm.entity.Person;
import com.brideside.crm.exception.BadRequestException;
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
import org.springframework.http.ResponseEntity;
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

    public PersonController(PersonService service) {
        this.service = service;
    }

    @Operation(summary = "List persons", description = "Search and filter persons by label, owner, organization, category, source, dealSource, and lead date range. Supports multi-select filtering for categoryId, organizationId, ownerId, and label using comma-separated values (e.g., categoryId=1,2,3&label=BRIDAL_MAKEUP,BRIDAL_PLANNING).")
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
            @ParameterObject @PageableDefault(size = 25, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable
    ) {
        List<Person.PersonLabel> labels = parseCommaSeparatedLabels(labelCode);
        com.brideside.crm.entity.DealSource source = parseSource(sourceCode);
        com.brideside.crm.entity.DealSource dealSource = parseDealSource(dealSourceCode);
        List<Long> organizationIds = parseCommaSeparatedIds(organizationId);
        List<Long> ownerIds = parseCommaSeparatedIds(ownerId);
        List<Long> categoryIds = parseCommaSeparatedIds(categoryId);
        return service.list(query, labels, organizationIds, ownerIds, categoryIds, source, dealSource, leadFrom, leadTo, pageable);
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

    @Operation(summary = "List eligible owners", description = "Returns active SALES users that can own a person")
    @GetMapping("/owners")
    public List<PersonDTO.OwnerOption> ownerOptions() {
        return service.listOwnerOptions();
    }

    @Operation(summary = "Get person by ID")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "200", description = "Person found")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "404", description = "Person not found")
    @GetMapping("/{id}")
    public PersonDTO get(@Parameter(description = "Person ID") @PathVariable Long id) {
        return service.get(id);
    }

    @Operation(summary = "Get person summary")
    @GetMapping("/{id}/summary")
    public PersonSummaryDTO getSummary(@PathVariable Long id) {
        return service.getSummary(id);
    }

    @Operation(summary = "Create person")
    @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "201", description = "Person created successfully")
    @PostMapping
    public ResponseEntity<PersonDTO> create(@Valid @RequestBody PersonDTO dto) {
        return ResponseEntity.status(201).body(service.create(dto));
    }

    @Operation(summary = "Update person")
    @PutMapping("/{id}")
    public PersonDTO update(@PathVariable Long id, @Valid @RequestBody PersonDTO dto) {
        return service.update(id, dto);
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

    @Operation(summary = "Merge duplicate persons")
    @PostMapping("/{id}/merge")
    public PersonDTO merge(@PathVariable Long id, @RequestBody MergeRequest request) {
        return service.merge(id, request != null ? request.getDuplicateIds() : List.of());
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

