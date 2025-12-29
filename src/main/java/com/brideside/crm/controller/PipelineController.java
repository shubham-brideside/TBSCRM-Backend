package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.PipelineDtos;
import com.brideside.crm.service.PipelineService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/pipelines")
@Tag(name = "Pipelines", description = "Pipelines and stages APIs aligned with Pipedrive-style workflows")
public class PipelineController {

    private final PipelineService pipelineService;

    public PipelineController(PipelineService pipelineService) {
        this.pipelineService = pipelineService;
    }

    @PostMapping
    @Operation(summary = "Create pipeline", description = "Creates a new pipeline with optional category, team, and organization.")
    public ResponseEntity<ApiResponse<PipelineDtos.PipelineResponse>> create(
            @Valid @RequestBody PipelineDtos.PipelineRequest request) {
        PipelineDtos.PipelineResponse response = pipelineService.createPipeline(request);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.success("Pipeline created", response));
    }

    @GetMapping
    @Operation(summary = "List pipelines", description = "Returns non-deleted pipelines sorted alphabetically. Optionally include stages.")
    public ResponseEntity<ApiResponse<List<PipelineDtos.PipelineResponse>>> list(
            @RequestParam(value = "includeStages", defaultValue = "false") boolean includeStages) {
        List<PipelineDtos.PipelineResponse> pipelines = pipelineService.listPipelines(includeStages);
        return ResponseEntity.ok(ApiResponse.success("Pipelines fetched", pipelines));
    }

    @GetMapping("/archived")
    @Operation(summary = "List archived pipelines", description = "Returns archived (deleted) pipelines sorted alphabetically. Optionally include stages.")
    public ResponseEntity<ApiResponse<List<PipelineDtos.PipelineResponse>>> listArchived(
            @RequestParam(value = "includeStages", defaultValue = "false") boolean includeStages) {
        List<PipelineDtos.PipelineResponse> pipelines = pipelineService.listArchivedPipelines(includeStages);
        return ResponseEntity.ok(ApiResponse.success("Archived pipelines fetched", pipelines));
    }

    @GetMapping("/categories")
    @Operation(summary = "List pipeline categories")
    public ResponseEntity<ApiResponse<List<PipelineDtos.CategoryOption>>> categories() {
        return ResponseEntity.ok(ApiResponse.success("Pipeline categories fetched", pipelineService.listCategoryOptions()));
    }

    @GetMapping("/{pipelineId}")
    @Operation(summary = "Get pipeline", description = "Fetch a single pipeline and optionally its stages.")
    public ResponseEntity<ApiResponse<PipelineDtos.PipelineResponse>> get(
            @PathVariable Long pipelineId,
            @RequestParam(value = "includeStages", defaultValue = "true") boolean includeStages) {
        PipelineDtos.PipelineResponse response = pipelineService.getPipeline(pipelineId, includeStages);
        return ResponseEntity.ok(ApiResponse.success("Pipeline fetched", response));
    }

    @PatchMapping("/{pipelineId}")
    @Operation(summary = "Update pipeline", description = "Partially updates pipeline name or organization.")
    public ResponseEntity<ApiResponse<PipelineDtos.PipelineResponse>> update(
            @PathVariable Long pipelineId,
            @Valid @RequestBody PipelineDtos.PipelineUpdateRequest request) {
        PipelineDtos.PipelineResponse response = pipelineService.updatePipeline(pipelineId, request);
        return ResponseEntity.ok(ApiResponse.success("Pipeline updated", response));
    }

    @DeleteMapping("/{pipelineId}")
    @Operation(summary = "Delete pipeline", description = "Deletes a pipeline. Pass hard=true to delete; other values are ignored.")
    public ResponseEntity<ApiResponse<Void>> delete(
            @PathVariable Long pipelineId,
            @RequestParam(value = "hard", defaultValue = "false") boolean hardDelete) {
        pipelineService.deletePipeline(pipelineId, hardDelete);
        return ResponseEntity.ok(ApiResponse.<Void>success("Pipeline deleted"));
    }

    @PatchMapping("/{pipelineId}/archive")
    @Operation(summary = "Archive pipeline", description = "Soft delete a pipeline by marking isDeleted=true.")
    public ResponseEntity<ApiResponse<PipelineDtos.PipelineResponse>> archive(@PathVariable Long pipelineId) {
        PipelineDtos.PipelineUpdateRequest request = new PipelineDtos.PipelineUpdateRequest();
        request.setDeleted(true);
        PipelineDtos.PipelineResponse response = pipelineService.updatePipeline(pipelineId, request);
        return ResponseEntity.ok(ApiResponse.success("Pipeline archived", response));
    }

    @PatchMapping("/{pipelineId}/unarchive")
    @Operation(summary = "Unarchive pipeline", description = "Restore an archived pipeline by marking isDeleted=false.")
    public ResponseEntity<ApiResponse<PipelineDtos.PipelineResponse>> unarchive(
            @PathVariable Long pipelineId,
            @RequestParam(value = "includeStages", defaultValue = "true") boolean includeStages) {
        PipelineDtos.PipelineResponse response = pipelineService.unarchivePipeline(pipelineId, includeStages);
        return ResponseEntity.ok(ApiResponse.success("Pipeline unarchived", response));
    }

    @GetMapping("/{pipelineId}/stages")
    @Operation(summary = "List stages", description = "List stages for a pipeline. includeInactive=true returns archived stages too.")
    public ResponseEntity<ApiResponse<List<PipelineDtos.StageResponse>>> listStages(
            @PathVariable Long pipelineId,
            @RequestParam(value = "includeInactive", defaultValue = "false") boolean includeInactive) {
        List<PipelineDtos.StageResponse> stages = pipelineService.listStages(pipelineId, includeInactive);
        return ResponseEntity.ok(ApiResponse.success("Stages fetched", stages));
    }

    @PostMapping("/{pipelineId}/stages")
    @Operation(summary = "Create stage", description = "Create a stage in the given pipeline at an optional order position.")
    public ResponseEntity<ApiResponse<PipelineDtos.StageResponse>> createStage(
            @PathVariable Long pipelineId,
            @Valid @RequestBody PipelineDtos.StageRequest request) {
        PipelineDtos.StageResponse response = pipelineService.createStage(pipelineId, request);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.success("Stage created", response));
    }

    @PatchMapping("/{pipelineId}/stages/{stageId}")
    @Operation(summary = "Update stage", description = "Partially updates stage details including name, probability, and order.")
    public ResponseEntity<ApiResponse<PipelineDtos.StageResponse>> updateStage(
            @PathVariable Long pipelineId,
            @PathVariable Long stageId,
            @Valid @RequestBody PipelineDtos.StageUpdateRequest request) {
        PipelineDtos.StageResponse response = pipelineService.updateStage(pipelineId, stageId, request);
        return ResponseEntity.ok(ApiResponse.success("Stage updated", response));
    }

    @DeleteMapping("/{pipelineId}/stages/{stageId}")
    @Operation(summary = "Delete stage", description = "Soft deletes a stage by default. Use hard=true to remove it permanently.")
    public ResponseEntity<ApiResponse<Void>> deleteStage(
            @PathVariable Long pipelineId,
            @PathVariable Long stageId,
            @RequestParam(value = "hard", defaultValue = "false") boolean hardDelete) {
        pipelineService.deleteStage(pipelineId, stageId, hardDelete);
        return ResponseEntity.ok(ApiResponse.<Void>success("Stage deleted"));
    }

    @PostMapping("/{pipelineId}/stages/reorder")
    @Operation(summary = "Reorder stages", description = "Reorders all stages in the pipeline based on the provided orderedStageIds list.")
    public ResponseEntity<ApiResponse<List<PipelineDtos.StageResponse>>> reorderStages(
            @PathVariable Long pipelineId,
            @Valid @RequestBody PipelineDtos.StageOrderRequest request) {
        List<PipelineDtos.StageResponse> stages = pipelineService.reorderStages(pipelineId, request);
        return ResponseEntity.ok(ApiResponse.success("Stage order updated", stages));
    }
}


