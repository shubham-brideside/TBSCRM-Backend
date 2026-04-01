package com.brideside.crm.dto;

import com.brideside.crm.entity.DealEditRequest;
import com.brideside.crm.entity.DealEditRequestStatus;
import com.fasterxml.jackson.annotation.JsonInclude;

import java.time.LocalDateTime;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class DealEditRequestDtos {

    public static class CreateRequest {
        public String reason;
        public DealDtos.UpdateRequest changes;
    }

    public static class DecisionRequest {
        public String adminComment;
    }

    public static class Response {
        public Long id;
        public Long dealId;
        public DealResponse currentDeal;
        public DealDtos.UpdateRequest changes;
        public String reason;
        public String status;
        public Long requestedByUserId;
        public String requestedByName;
        public Long processedByUserId;
        public String processedByName;
        public String adminComment;
        public LocalDateTime createdAt;
        public LocalDateTime processedAt;

        public static Response fromEntity(DealEditRequest entity,
                                          DealResponse currentDeal,
                                          DealDtos.UpdateRequest changes) {
            Response r = new Response();
            r.id = entity.getId();
            r.dealId = entity.getDeal() != null ? entity.getDeal().getId() : null;
            r.currentDeal = currentDeal;
            r.changes = changes;
            r.reason = entity.getReason();
            DealEditRequestStatus status = entity.getStatus();
            r.status = status != null ? status.name() : null;
            if (entity.getRequestedBy() != null) {
                r.requestedByUserId = entity.getRequestedBy().getId();
                r.requestedByName = entity.getRequestedBy().getDisplayName();
            }
            if (entity.getProcessedBy() != null) {
                r.processedByUserId = entity.getProcessedBy().getId();
                r.processedByName = entity.getProcessedBy().getDisplayName();
            }
            r.adminComment = entity.getAdminComment();
            r.createdAt = entity.getCreatedAt();
            r.processedAt = entity.getProcessedAt();
            return r;
        }
    }
}

