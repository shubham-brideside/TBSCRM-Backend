package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.SimpleDtos;
import com.brideside.crm.entity.Source;
import com.brideside.crm.service.SourceService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/sources")
@Tag(name = "Sources", description = "Lead source APIs")
public class SourceController {

    @Autowired private SourceService sourceService;

    @PostMapping
    @Operation(summary = "Create source")
    public ResponseEntity<ApiResponse<Source>> create(@Valid @RequestBody SimpleDtos.SourceCreate req) {
        return ResponseEntity.ok(ApiResponse.success("Source created", sourceService.create(req)));
    }

    @GetMapping
    @Operation(summary = "List sources")
    public ResponseEntity<ApiResponse<List<Source>>> list() {
        return ResponseEntity.ok(ApiResponse.success("Sources fetched", sourceService.list()));
    }
}



