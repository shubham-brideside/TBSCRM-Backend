package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
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

import java.util.Arrays;
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
    @Operation(summary = "List deals", description = "List all deals with optional sorting. Use 'sort' query parameter in format 'field,direction' (e.g., 'name,asc' or 'value,desc'). Default: 'nextActivity,asc'")
    public ResponseEntity<List<DealResponse>> list(
            @RequestParam(required = false, defaultValue = "nextActivity,asc") String sort) {
        String[] sortParts = sort.split(",");
        String sortField = sortParts.length > 0 ? sortParts[0].trim() : "nextActivity";
        String sortDirection = sortParts.length > 1 ? sortParts[1].trim() : "asc";
        
        List<DealResponse> res = dealService.list(sortField, sortDirection).stream()
            .map(this::toResponse)
            .collect(Collectors.toList());
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

    @PatchMapping("/{id}")
    @Operation(summary = "Update deal details", description = "Partially update deal fields. Only provided fields will be updated.")
    public ResponseEntity<DealResponse> update(@PathVariable Long id, @Valid @RequestBody DealDtos.UpdateRequest req) {
        Deal d = dealService.update(id, req);
        return ResponseEntity.ok(toResponse(d));
    }

    @PutMapping("/{id}/stage")
    @Operation(summary = "Move deal to another stage")
    public ResponseEntity<DealResponse> updateStage(@PathVariable Long id, @RequestBody DealDtos.UpdateStageRequest req) {
        return ResponseEntity.ok(toResponse(dealService.updateStage(id, req)));
    }

    @PatchMapping("/{id}/status")
    @Operation(summary = "Update deal status", description = "Update deal status. When marking as LOST, lostReason is required.")
    public ResponseEntity<DealResponse> markStatus(@PathVariable Long id, @Valid @RequestBody DealDtos.MarkStatusRequest req) {
        return ResponseEntity.ok(toResponse(dealService.markStatus(id, req)));
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "Delete deal")
    public ResponseEntity<Void> delete(@PathVariable Long id) {
        dealService.delete(id);
        return ResponseEntity.noContent().build();
    }

    private DealResponse toResponse(Deal d) {
        DealResponse r = new DealResponse();
        r.id = d.getId();
        r.name = d.getName();
        r.value = d.getValue();
        r.personId = d.getPerson() != null ? d.getPerson().getId() : null;
        r.personName = d.getPerson() != null ? d.getPerson().getName() : null;
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
        r.venue = d.getVenue();
        r.phoneNumber = d.getPhoneNumber();
        r.finalThankYouSent = d.getFinalThankYouSent();
        r.eventDateAsked = d.getEventDateAsked();
        r.contactNumberAsked = d.getContactNumberAsked();
        r.venueAsked = d.getVenueAsked();
        r.eventDate = d.getEventDate() != null ? d.getEventDate().toString() : null;
        r.label = d.getLabel() != null ? d.getLabel().toDisplayString() : null;
        r.source = d.getDealSource() != null ? d.getDealSource().toDisplayString() : null;
        r.subSource = d.getDealSubSource() != null ? d.getDealSubSource().toDisplayString() : null;
        r.isDiverted = d.getIsDiverted();
        r.referencedDealId = d.getReferencedDeal() != null ? d.getReferencedDeal().getId() : null;
        r.referencedPipelineId = d.getReferencedPipeline() != null ? d.getReferencedPipeline().getId() : null;
        r.sourcePipelineId = d.getSourcePipeline() != null ? d.getSourcePipeline().getId() : null;
        r.pipelineHistory = d.getPipelineHistory();
        r.isDeleted = d.getIsDeleted();
        r.lostReason = d.getLostReason() != null ? d.getLostReason().toDisplayString() : null;
        return r;
    }

    @GetMapping("/{dealId}/available-pipelines")
    @Operation(summary = "Get available pipelines for diversion", description = "Returns pipelines where the deal has not been diverted yet")
    public ResponseEntity<List<com.brideside.crm.dto.PipelineDtos.PipelineResponse>> getAvailablePipelinesForDiversion(@PathVariable Long dealId) {
        List<com.brideside.crm.dto.PipelineDtos.PipelineResponse> pipelines = dealService.getAvailablePipelinesForDiversion(dealId);
        return ResponseEntity.ok(pipelines);
    }

    @GetMapping("/sources")
    @Operation(summary = "Get deal sources", description = "Returns list of available deal sources")
    public ResponseEntity<ApiResponse<List<SourceOption>>> getSources() {
        List<SourceOption> sources = Arrays.asList(
            new SourceOption("Direct", "Direct"),
            new SourceOption("Divert", "Divert"),
            new SourceOption("Reference", "Reference"),
            new SourceOption("Planner", "Planner")
        );
        return ResponseEntity.ok(ApiResponse.success("Sources retrieved successfully", sources));
    }

    @GetMapping("/sub-sources")
    @Operation(summary = "Get deal sub sources", description = "Returns list of available deal sub sources (for Direct source)")
    public ResponseEntity<ApiResponse<List<SourceOption>>> getSubSources() {
        List<SourceOption> subSources = Arrays.asList(
            new SourceOption("Instagram", "Instagram"),
            new SourceOption("Whatsapp", "Whatsapp"),
            new SourceOption("Landing Page", "Landing Page"),
            new SourceOption("Email", "Email")
        );
        return ResponseEntity.ok(ApiResponse.success("Sub sources retrieved successfully", subSources));
    }

    public static class SourceOption {
        private String code;
        private String label;

        public SourceOption(String code, String label) {
            this.code = code;
            this.label = label;
        }

        public String getCode() {
            return code;
        }

        public void setCode(String code) {
            this.code = code;
        }

        public String getLabel() {
            return label;
        }

        public void setLabel(String label) {
            this.label = label;
        }
    }
}


