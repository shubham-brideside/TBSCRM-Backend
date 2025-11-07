package com.brideside.crm.controller;

import com.brideside.crm.dto.ActivityDtos;
import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.entity.Activity;
import com.brideside.crm.service.ActivityService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/activities")
@Tag(name = "Activities", description = "Deal activity APIs")
public class ActivityController {

    @Autowired private ActivityService activityService;

    @PostMapping
    @Operation(summary = "Create activity for a deal")
    public ResponseEntity<ApiResponse<Activity>> create(@Valid @RequestBody ActivityDtos.CreateRequest req) {
        return ResponseEntity.ok(ApiResponse.success("Activity created", activityService.create(req)));
    }

    @GetMapping("/deal/{dealId}")
    @Operation(summary = "List activities for a deal")
    public ResponseEntity<ApiResponse<List<Activity>>> listByDeal(@PathVariable Long dealId) {
        return ResponseEntity.ok(ApiResponse.success("Activities fetched", activityService.listByDeal(dealId)));
    }
}



