package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.TeamDtos;
import com.brideside.crm.service.TeamService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/teams")
@Tag(name = "Teams", description = "Team management APIs")
public class TeamController {

    private final TeamService teamService;

    public TeamController(TeamService teamService) {
        this.teamService = teamService;
    }

    @PostMapping
    @Operation(summary = "Create team")
    public ResponseEntity<ApiResponse<TeamDtos.TeamResponse>> create(
            @Valid @RequestBody TeamDtos.TeamRequest request) {
        return ResponseEntity.status(201)
                .body(ApiResponse.success("Team created", teamService.create(request)));
    }

    @GetMapping
    @Operation(summary = "List teams")
    public ResponseEntity<ApiResponse<List<TeamDtos.TeamResponse>>> list() {
        return ResponseEntity.ok(ApiResponse.success("Teams fetched", teamService.list()));
    }

    @GetMapping("/managers")
    @Operation(summary = "List eligible team managers based on target role")
    public ResponseEntity<ApiResponse<List<TeamDtos.UserSummary>>> managers(
            @RequestParam(name = "forRole", required = false) String forRole) {
        com.brideside.crm.entity.Role.RoleName roleName = null;
        if (forRole != null && !forRole.isBlank()) {
            try {
                roleName = com.brideside.crm.entity.Role.RoleName.valueOf(forRole.toUpperCase());
            } catch (IllegalArgumentException ignored) {
                return ResponseEntity.badRequest()
                        .body(ApiResponse.error("Invalid role value. Allowed: ADMIN, CATEGORY_MANAGER, SALES, PRESALES"));
            }
        }
        return ResponseEntity.ok(ApiResponse.success("Team managers fetched", teamService.listManagersForRole(roleName)));
    }

    @GetMapping("/members")
    @Operation(summary = "List eligible team members (PRESALES role)")
    public ResponseEntity<ApiResponse<List<TeamDtos.UserSummary>>> members() {
        return ResponseEntity.ok(ApiResponse.success("Team members fetched", teamService.listMembers()));
    }

    @GetMapping("/{id}")
    @Operation(summary = "Get team")
    public ResponseEntity<ApiResponse<TeamDtos.TeamResponse>> get(@PathVariable("id") Long id) {
        return ResponseEntity.ok(ApiResponse.success("Team fetched", teamService.get(id)));
    }

    @PutMapping("/{id}")
    @Operation(summary = "Update team")
    public ResponseEntity<ApiResponse<TeamDtos.TeamResponse>> update(
            @PathVariable("id") Long id,
            @Valid @RequestBody TeamDtos.TeamRequest request) {
        return ResponseEntity.ok(ApiResponse.success("Team updated", teamService.update(id, request)));
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "Delete team")
    public ResponseEntity<ApiResponse<Void>> delete(@PathVariable("id") Long id) {
        teamService.delete(id);
        return ResponseEntity.ok(ApiResponse.success("Team deleted"));
    }
}
