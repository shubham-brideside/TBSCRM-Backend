package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.LabelDtos.CreateLabelRequest;
import com.brideside.crm.dto.LabelDtos.UpdateLabelRequest;
import com.brideside.crm.entity.Label;
import com.brideside.crm.service.LabelService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/labels")
@Tag(name = "Labels", description = "Label management APIs for deals")
public class LabelController {

    @Autowired
    private LabelService labelService;

    @GetMapping
    @Operation(summary = "Get all labels", description = "Fetch all labels. Can optionally filter by organization ID or pipeline ID.")
    @ApiResponses(value = {
        @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "200", description = "Labels fetched successfully")
    })
    public ResponseEntity<ApiResponse<List<Label>>> getAllLabels(
            @Parameter(description = "Filter by organization ID") @RequestParam(required = false) Long organizationId,
            @Parameter(description = "Filter by pipeline ID") @RequestParam(required = false) Long pipelineId,
            @Parameter(description = "Get only global labels") @RequestParam(required = false) Boolean globalOnly) {
        
        List<Label> labels;
        
        if (Boolean.TRUE.equals(globalOnly)) {
            labels = labelService.findGlobalLabels();
        } else if (organizationId != null) {
            labels = labelService.findByOrganizationId(organizationId);
        } else if (pipelineId != null) {
            labels = labelService.findByPipelineId(pipelineId);
        } else {
            labels = labelService.findAll();
        }
        
        return ResponseEntity.ok(ApiResponse.success("Labels fetched successfully", labels));
    }

    @GetMapping("/{id}")
    @Operation(summary = "Get label by ID", description = "Fetch a single label by its ID")
    @ApiResponses(value = {
        @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "200", description = "Label fetched successfully"),
        @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "404", description = "Label not found")
    })
    public ResponseEntity<ApiResponse<Label>> getLabelById(
            @Parameter(description = "Label ID") @PathVariable Long id) {
        return labelService.findById(id)
                .map(label -> ResponseEntity.ok(ApiResponse.success("Label fetched successfully", label)))
                .orElse(ResponseEntity.status(HttpStatus.NOT_FOUND)
                        .body(ApiResponse.error("Label not found with id: " + id)));
    }

    @PostMapping
    @Operation(summary = "Create a new label", description = "Create a new customizable label with name and color")
    @ApiResponses(value = {
        @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "201", description = "Label created successfully"),
        @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "400", description = "Invalid request data")
    })
    public ResponseEntity<ApiResponse<Label>> createLabel(
            @Valid @RequestBody CreateLabelRequest request) {
        Label label = labelService.create(request);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.success("Label created successfully", label));
    }

    @PutMapping("/{id}")
    @Operation(summary = "Update a label", description = "Update an existing label's name, color, or other properties")
    @ApiResponses(value = {
        @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "200", description = "Label updated successfully"),
        @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "404", description = "Label not found")
    })
    public ResponseEntity<ApiResponse<Label>> updateLabel(
            @Parameter(description = "Label ID") @PathVariable Long id,
            @Valid @RequestBody UpdateLabelRequest request) {
        try {
            Label label = labelService.update(id, request);
            return ResponseEntity.ok(ApiResponse.success("Label updated successfully", label));
        } catch (RuntimeException e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ApiResponse.error(e.getMessage()));
        }
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "Delete a label", description = "Delete a label by its ID")
    @ApiResponses(value = {
        @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "200", description = "Label deleted successfully"),
        @io.swagger.v3.oas.annotations.responses.ApiResponse(responseCode = "404", description = "Label not found")
    })
    public ResponseEntity<ApiResponse<Void>> deleteLabel(
            @Parameter(description = "Label ID") @PathVariable Long id) {
        try {
            labelService.delete(id);
            return ResponseEntity.ok(ApiResponse.success("Label deleted successfully"));
        } catch (RuntimeException e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(ApiResponse.error(e.getMessage()));
        }
    }
}
