package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.SimpleDtos;
import com.brideside.crm.entity.Pipeline;
import com.brideside.crm.entity.Stage;
import com.brideside.crm.service.PipelineService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/pipelines")
@Tag(name = "Pipelines", description = "Pipelines and stages APIs")
public class PipelineController {

    @Autowired private PipelineService pipelineService;

    @PostMapping
    @Operation(summary = "Create pipeline")
    public ResponseEntity<ApiResponse<Pipeline>> create(@Valid @RequestBody SimpleDtos.PipelineCreate req) {
        return ResponseEntity.ok(ApiResponse.success("Pipeline created", pipelineService.create(req)));
    }

    @GetMapping
    @Operation(summary = "List pipelines")
    public ResponseEntity<ApiResponse<List<Pipeline>>> list() {
        return ResponseEntity.ok(ApiResponse.success("Pipelines fetched", pipelineService.list()));
    }

    @PostMapping("/stages")
    @Operation(summary = "Add stage to pipeline")
    public ResponseEntity<ApiResponse<Stage>> addStage(@Valid @RequestBody SimpleDtos.StageCreate req) {
        return ResponseEntity.ok(ApiResponse.success("Stage created", pipelineService.addStage(req)));
    }

    @GetMapping("/{pipelineId}/stages")
    @Operation(summary = "List stages for pipeline")
    public ResponseEntity<ApiResponse<List<Stage>>> listStages(@PathVariable Long pipelineId) {
        return ResponseEntity.ok(ApiResponse.success("Stages fetched", pipelineService.listStages(pipelineId)));
    }
}



