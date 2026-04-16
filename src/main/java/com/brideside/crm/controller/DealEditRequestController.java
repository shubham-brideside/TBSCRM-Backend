package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.DealDtos;
import com.brideside.crm.dto.DealEditRequestDtos;
import com.brideside.crm.dto.DealResponse;
import com.brideside.crm.entity.DealEditRequest;
import com.brideside.crm.service.DealEditRequestService;
import com.brideside.crm.service.DealService;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/deals")
@Tag(name = "Deal Edit Requests", description = "Edit requests for won deals")
public class DealEditRequestController {

    @Autowired
    private DealEditRequestService dealEditRequestService;

    @Autowired
    private DealService dealService;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @PostMapping("/{dealId}/edit-requests")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "Create edit request for a won deal",
            description = "Creates an edit request for a WON deal. Admin must approve before changes are applied.")
    public ResponseEntity<ApiResponse<DealEditRequestDtos.Response>> create(
            @PathVariable Long dealId,
            @Valid @RequestBody DealEditRequestDtos.CreateRequest request) {

        DealEditRequest entity = dealEditRequestService.create(dealId, request);
        DealResponse currentDeal = toDealResponse(dealService.get(dealId));
        DealEditRequestDtos.Response body = DealEditRequestDtos.Response.fromEntity(
                entity,
                currentDeal,
                deserializeRequestedChanges(entity.getRequestedChanges())
        );
        return ResponseEntity.ok(ApiResponse.success("Edit request created", body));
    }

    @GetMapping("/edit-requests/pending")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "List pending deal edit requests",
            description = "Returns all pending edit requests for won deals.")
    public ResponseEntity<ApiResponse<List<DealEditRequestDtos.Response>>> listPending() {
        List<DealEditRequest> entities = dealEditRequestService.listPending();
        List<DealEditRequestDtos.Response> body = entities.stream()
                .map(e -> {
                    DealResponse dealResponse = toDealResponse(e.getDeal());
                    DealDtos.UpdateRequest changes = deserializeRequestedChanges(e.getRequestedChanges());
                    return DealEditRequestDtos.Response.fromEntity(e, dealResponse, changes);
                })
                .collect(Collectors.toList());
        return ResponseEntity.ok(ApiResponse.success("Pending edit requests fetched", body));
    }

    @PostMapping("/edit-requests/{requestId}/accept")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Accept deal edit request",
            description = "Applies requested changes to the deal and marks request as PERMITTED.")
    public ResponseEntity<ApiResponse<DealEditRequestDtos.Response>> accept(
            @PathVariable Long requestId,
            @RequestBody(required = false) DealEditRequestDtos.DecisionRequest request) {

        DealEditRequest entity = dealEditRequestService.permit(requestId, request);
        DealResponse currentDeal = toDealResponse(entity.getDeal());
        DealDtos.UpdateRequest changes = deserializeRequestedChanges(entity.getRequestedChanges());
        DealEditRequestDtos.Response body = DealEditRequestDtos.Response.fromEntity(entity, currentDeal, changes);
        return ResponseEntity.ok(ApiResponse.success("Edit request accepted", body));
    }

    @PostMapping("/edit-requests/{requestId}/reject")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "Reject deal edit request",
            description = "Marks the edit request as REJECTED. Deal is not changed.")
    public ResponseEntity<ApiResponse<DealEditRequestDtos.Response>> reject(
            @PathVariable Long requestId,
            @RequestBody(required = false) DealEditRequestDtos.DecisionRequest request) {

        DealEditRequest entity = dealEditRequestService.reject(requestId, request);
        DealResponse currentDeal = toDealResponse(entity.getDeal());
        DealDtos.UpdateRequest changes = deserializeRequestedChanges(entity.getRequestedChanges());
        DealEditRequestDtos.Response body = DealEditRequestDtos.Response.fromEntity(entity, currentDeal, changes);
        return ResponseEntity.ok(ApiResponse.success("Edit request rejected", body));
    }

    private DealDtos.UpdateRequest deserializeRequestedChanges(String requestedChanges) {
        if (requestedChanges == null || requestedChanges.isBlank()) {
            return null;
        }
        try {
            return objectMapper.readValue(requestedChanges, DealDtos.UpdateRequest.class);
        } catch (Exception ignored) {
            return null;
        }
    }

    private DealResponse toDealResponse(com.brideside.crm.entity.Deal d) {
        // Simple mapper mirroring DealController.toResponse to avoid changing Deal DTOs/entities
        DealResponse r = new DealResponse();
        r.id = d.getId();
        r.name = d.getName();
        r.value = d.getValue();
        r.personId = d.getPersonId();
        r.personName = d.getPersonName();
        r.pipelineId = d.getPipeline() != null ? d.getPipeline().getId() : null;
        r.stageId = d.getStage() != null ? d.getStage().getId() : null;
        r.sourceId = d.getSource() != null ? d.getSource().getId() : null;
        r.organizationId = d.getOrganization() != null ? d.getOrganization().getId() : null;
        r.organizationName = d.getOrganization() != null ? d.getOrganization().getName() : null;
        r.categoryId = d.getDealCategory() != null ? d.getDealCategory().getId() : null;
        r.eventType = d.getEventType();
        r.status = d.getStatus();
        r.commissionAmount = d.getCommissionAmount();
        r.createdAt = d.getCreatedAt();
        r.updatedAt = d.getUpdatedAt();
        r.venue = d.getVenue();
        r.phoneNumber = d.getPhoneNumber();
        r.city = d.getCity();
        r.notes = d.getNotes();
        r.finalThankYouSent = d.getFinalThankYouSent();
        r.eventDateAsked = d.getEventDateAsked();
        r.contactNumberAsked = d.getContactNumberAsked();
        r.venueAsked = d.getVenueAsked();
        r.eventDate = d.getEventDate() != null ? d.getEventDate().toString() : null;
        r.eventDates = null;
        r.isDeleted = d.getIsDeleted();
        r.clientBudget = d.getClientBudget();
        r.wonAt = d.getWonAt();
        r.lostAt = d.getLostAt();
        r.createdBy = d.getCreatedBy();
        r.createdByUserId = d.getCreatedByUserId();
        r.createdByName = d.getCreatedByName();
        r.divertedByUserId = d.getDivertedByUserId();
        r.divertedByName = d.getDivertedByName();
        if (d.getOwner() != null) {
            r.ownerId = d.getOwner().getId();
            r.ownerDisplayName = d.getOwner().getDisplayName();
        } else if (d.getOwnerId() != null) {
            r.ownerId = d.getOwnerId();
        }
        return r;
    }
}

