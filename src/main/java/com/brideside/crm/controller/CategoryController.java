package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.CategoryDtos;
import com.brideside.crm.dto.PipelineDtos;
import com.brideside.crm.service.CategoryService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/categories")
@Tag(name = "Categories", description = "Category-based filtering APIs")
public class CategoryController {

    private final CategoryService categoryService;

    public CategoryController(CategoryService categoryService) {
        this.categoryService = categoryService;
    }

    @GetMapping("/{categoryId}/managers")
    @Operation(
            summary = "Get managers by category",
            description = "Returns all managers (users) that are associated with deals/pipelines in the specified category. " +
                    "Category can be identified by ID (numeric) or name (string). " +
                    "Managers include team managers of pipelines and person owners of deals in the category."
    )
    public ResponseEntity<ApiResponse<List<CategoryDtos.ManagerResponse>>> getManagersByCategory(
            @PathVariable String categoryId) {
        List<CategoryDtos.ManagerResponse> managers = categoryService.getManagersByCategory(categoryId);
        return ResponseEntity.ok(ApiResponse.success("Managers fetched", managers));
    }

    @GetMapping("/{categoryId}/pipelines")
    @Operation(
            summary = "Get pipelines by category",
            description = "Returns all pipelines that belong to the specified category, filtered by user permissions. " +
                    "Category can be identified by ID (numeric) or name (string). " +
                    "Optionally include stages in the response."
    )
    public ResponseEntity<ApiResponse<List<PipelineDtos.PipelineResponse>>> getPipelinesByCategory(
            @PathVariable String categoryId,
            @RequestParam(value = "includeStages", defaultValue = "false") boolean includeStages) {
        List<PipelineDtos.PipelineResponse> pipelines = categoryService.getPipelinesByCategory(categoryId, includeStages);
        return ResponseEntity.ok(ApiResponse.success("Pipelines fetched", pipelines));
    }
}

