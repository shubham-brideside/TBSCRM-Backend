package com.brideside.crm.controller;

import com.brideside.crm.dto.DealDtos;
import com.brideside.crm.dto.DealResponse;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.DealStatus;
import com.brideside.crm.service.DealService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/deals")
@Tag(name = "Deals", description = "Deal management APIs")
public class DealController {

    @Autowired
    private DealService dealService;

    @PostMapping
    @Operation(summary = "Create a deal")
    public ResponseEntity<DealResponse> create(@Valid @RequestBody DealDtos.CreateRequest req) {
        Deal d = dealService.create(req);
        return ResponseEntity.ok(toResponse(d));
    }

    @GetMapping
    @Operation(summary = "List deals")
    public ResponseEntity<List<DealResponse>> list() {
        List<DealResponse> res = dealService.list().stream().map(this::toResponse).collect(Collectors.toList());
        return ResponseEntity.ok(res);
    }

    @GetMapping("/won")
    @Operation(summary = "List won deals")
    public ResponseEntity<List<DealResponse>> listWon() {
        List<DealResponse> res = dealService.listWon().stream().map(this::toResponse).collect(Collectors.toList());
        return ResponseEntity.ok(res);
    }

    @GetMapping("/lost")
    @Operation(summary = "List lost deals")
    public ResponseEntity<List<DealResponse>> listLost() {
        List<DealResponse> res = dealService.listByStatus(DealStatus.LOST).stream().map(this::toResponse).collect(Collectors.toList());
        return ResponseEntity.ok(res);
    }

    @GetMapping("/inprogress")
    @Operation(summary = "List in-progress deals")
    public ResponseEntity<List<DealResponse>> listInProgress() {
        List<DealResponse> res = dealService.listByStatus(DealStatus.IN_PROGRESS).stream().map(this::toResponse).collect(Collectors.toList());
        return ResponseEntity.ok(res);
    }

    @GetMapping("/person/{personId}")
    @Operation(summary = "List deals for a person")
    public ResponseEntity<List<DealResponse>> listByPerson(@PathVariable Long personId) {
        List<DealResponse> res = dealService.listByPerson(personId).stream().map(this::toResponse).collect(Collectors.toList());
        return ResponseEntity.ok(res);
    }

    @GetMapping("/organization/{organizationId}")
    @Operation(summary = "List deals for an organization")
    public ResponseEntity<List<DealResponse>> listByOrganization(@PathVariable Long organizationId) {
        List<DealResponse> res = dealService.listByOrganization(organizationId).stream().map(this::toResponse).collect(Collectors.toList());
        return ResponseEntity.ok(res);
    }

    @GetMapping("/category/{categoryId}")
    @Operation(summary = "List deals for a category")
    public ResponseEntity<List<DealResponse>> listByCategory(@PathVariable Long categoryId) {
        List<DealResponse> res = dealService.listByCategory(categoryId).stream().map(this::toResponse).collect(Collectors.toList());
        return ResponseEntity.ok(res);
    }

    @GetMapping("/{id}")
    @Operation(summary = "Get deal by id")
    public ResponseEntity<DealResponse> get(@PathVariable Long id) {
        return ResponseEntity.ok(toResponse(dealService.get(id)));
    }

    @PutMapping("/{id}/stage")
    @Operation(summary = "Move deal to another stage")
    public ResponseEntity<DealResponse> updateStage(@PathVariable Long id, @RequestBody DealDtos.UpdateStageRequest req) {
        return ResponseEntity.ok(toResponse(dealService.updateStage(id, req)));
    }

    @PatchMapping("/{id}/status")
    @Operation(summary = "Update deal status")
    public ResponseEntity<DealResponse> markStatus(@PathVariable Long id, @RequestBody DealDtos.MarkStatusRequest req) {
        return ResponseEntity.ok(toResponse(dealService.markStatus(id, req.status)));
    }

    private DealResponse toResponse(Deal d) {
        DealResponse r = new DealResponse();
        r.id = d.getId();
        r.name = d.getName();
        r.value = d.getValue();
        r.personId = d.getPerson() != null ? d.getPerson().getId() : null;
        r.pipelineId = d.getPipeline() != null ? d.getPipeline().getId() : null;
        r.stageId = d.getStage() != null ? d.getStage().getId() : null;
        r.sourceId = d.getSource() != null ? d.getSource().getId() : null;
        r.organizationId = d.getOrganization() != null ? d.getOrganization().getId() : null;
        r.categoryId = d.getDealCategory() != null ? d.getDealCategory().getId() : null;
        r.eventType = d.getEventType();
        r.status = d.getStatus();
        r.commissionAmount = d.getCommissionAmount();
        r.createdAt = d.getCreatedAt();
        r.venue = d.getVenue();
        r.phoneNumber = d.getPhoneNumber();
        r.finalThankYouSent = d.getFinalThankYouSent();
        r.eventDateAsked = d.getEventDateAsked();
        r.contactNumberAsked = d.getContactNumberAsked();
        r.venueAsked = d.getVenueAsked();
        r.eventDate = d.getEventDate() != null ? d.getEventDate().toString() : null;
        return r;
    }
}


