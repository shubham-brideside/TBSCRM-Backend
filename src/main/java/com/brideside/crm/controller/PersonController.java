package com.brideside.crm.controller;

import com.brideside.crm.dto.MergeRequest;
import com.brideside.crm.dto.PersonDTO;
import com.brideside.crm.dto.PersonSummaryDTO;
import com.brideside.crm.entity.Person;
import com.brideside.crm.exception.BadRequestException;
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
import org.springframework.data.web.PageableDefault;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.List;

@RestController
@RequestMapping("/api/persons")
@Tag(name = "Persons", description = "API for managing leads/person records")
public class PersonController {

    private final PersonService service;

    public PersonController(PersonService service) {
        this.service = service;
    }

    @Operation(summary = "List persons", description = "Search and filter persons by label, owner, organization, source, and lead date range.")
    @GetMapping
    public Page<PersonDTO> list(
            @RequestParam(name = "q", required = false) String query,
            @RequestParam(name = "label", required = false) String labelCode,
            @RequestParam(name = "source", required = false) String sourceCode,
            @RequestParam(name = "organizationId", required = false) Long organizationId,
            @RequestParam(name = "ownerId", required = false) Long ownerId,
            @RequestParam(name = "leadFrom", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate leadFrom,
            @RequestParam(name = "leadTo", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate leadTo,
            @ParameterObject @PageableDefault(size = 25, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable
    ) {
        Person.PersonLabel label = parseLabel(labelCode);
        Person.PersonSource source = parseSource(sourceCode);
        return service.list(query, label, organizationId, ownerId, source, leadFrom, leadTo, pageable);
    }

    @Operation(summary = "List label options")
    @GetMapping("/labels")
    public List<PersonDTO.EnumOption> labelOptions() {
        return service.listLabelOptions();
    }

    @Operation(summary = "List source options")
    @GetMapping("/sources")
    public List<PersonDTO.EnumOption> sourceOptions() {
        return service.listSourceOptions();
    }

    @Operation(summary = "List eligible owners", description = "Returns active SALES users that can own a person")
    @GetMapping("/owners")
    public List<PersonDTO.OwnerOption> ownerOptions() {
        return service.listOwnerOptions();
    }

    @Operation(summary = "Get person by ID")
    @ApiResponse(responseCode = "200", description = "Person found")
    @ApiResponse(responseCode = "404", description = "Person not found")
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
    @ApiResponse(responseCode = "201", description = "Person created successfully")
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
    @ApiResponse(responseCode = "204", description = "Person deleted successfully")
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

    private Person.PersonSource parseSource(String value) {
        if (value == null || value.isBlank()) {
            return null;
        }
        try {
            return Person.PersonSource.valueOf(value.toUpperCase());
        } catch (IllegalArgumentException ex) {
            throw new BadRequestException("Invalid source value: " + value);
        }
    }
}

