package com.brideside.crm.dto;

import java.math.BigDecimal;
import java.util.List;
import com.brideside.crm.entity.CreatedByType;
import com.brideside.crm.entity.DealStatus;

public class DealDtos {
    public static class CreateRequest {
        public String name;
        public BigDecimal value;
        public Long personId;
        public Long pipelineId;
        public Long stageId;
        public Long sourceId;
        public Long organizationId;
        /**
         * Optional category identifier. Accepts either a numeric category id or a string code such as "PHOTOGRAPHY".
         */
        public String categoryId;
        public String category;
        public String eventType;
        public DealStatus status; // optional
        public BigDecimal commissionAmount; // optional
        public String venue; // optional
        public String phoneNumber; // optional
        public String city; // optional
        public Boolean finalThankYouSent; // optional
        public Boolean eventDateAsked; // optional
        public Boolean contactNumberAsked; // optional
        public Boolean venueAsked; // optional
        public String eventDate; // ISO-8601 date string (yyyy-MM-dd) - legacy, use eventDates instead
        public List<String> eventDates; // List of ISO-8601 date strings (yyyy-MM-dd)
        public String label; // optional: DIRECT, DIVERT, DESTINATION, PARTY MAKEUP, PRE WEDDING, BRIDAL MAKEUP (legacy enum)
        public Long labelId; // optional: ID of custom label from labels table (deprecated, use labelIds instead)
        public List<Long> labelIds; // optional: List of label IDs from labels table
        public String source; // optional: Direct, Divert, Reference, Planner
        public String subSource; // optional: Instagram, Whatsapp, Landing Page, Email (only valid when source is "Direct")
        public Long referencedDealId; // optional: ID of the original deal when diverting (required when label is DIVERT)
        public CreatedByType createdBy; // optional: USER (default) or BOT
        public Long createdByUserId; // optional: User ID when createdBy is USER
    }

    public static class UpdateStageRequest {
        public Long stageId;
    }

    public static class MarkStatusRequest {
        public DealStatus status;
        public String lostReason; // optional: Required when status is LOST. Values: "Slot not opened", "Not Interested", "Date postponed", "Not Available", "Ghosted", "Budget", "Booked Someone else"
        public BigDecimal value; // optional: Required when status is WON and deal doesn't have a value. Can be edited if deal already has a value.
        public BigDecimal commissionAmount; // optional: Commission amount. Default: 10% of value for Direct/Reference/Planner, 15% for Divert. Can be edited.
        public BigDecimal clientBudget; // optional: Required when lostReason is "Budget". Client's budget amount.
    }

    public static class UpdateRequest {
        public String name;
        public BigDecimal value;
        public Long personId;
        public Long pipelineId;
        public Long stageId;
        public Long sourceId;
        public Long organizationId;
        /**
         * Optional category identifier. Accepts either a numeric category id or a string code such as "PHOTOGRAPHY".
         */
        public String categoryId;
        public String category;
        public String eventType;
        public DealStatus status;
        public BigDecimal commissionAmount;
        public String venue;
        public String phoneNumber;
        public String city;
        public Boolean finalThankYouSent;
        public Boolean eventDateAsked;
        public Boolean contactNumberAsked;
        public Boolean venueAsked;
        public String eventDate; // ISO-8601 date string (yyyy-MM-dd) - legacy, use eventDates instead
        public List<String> eventDates; // List of ISO-8601 date strings (yyyy-MM-dd)
        public String label; // optional: DIRECT, DIVERT, DESTINATION, PARTY MAKEUP, PRE WEDDING, BRIDAL MAKEUP (legacy enum)
        public Long labelId; // optional: ID of custom label from labels table (deprecated, use labelIds instead)
        public List<Long> labelIds; // optional: List of label IDs from labels table
        public String source; // optional: Direct, Divert, Reference, Planner
        public String subSource; // optional: Instagram, Whatsapp, Landing Page, Email (only valid when source is "Direct")
        public BigDecimal clientBudget; // optional: Client's budget amount (typically set when lost reason is "Budget")
    }

    public static class ListResponse {
        public List<DealResponse> deals;
        public Long totalCount;
        public List<PersonDTO> persons; // All persons linked to the returned deals
        public List<ActivityDTO> activities; // All activities linked to the returned deals

        public ListResponse(List<DealResponse> deals, Long totalCount, 
                           List<PersonDTO> persons, List<ActivityDTO> activities) {
            this.deals = deals;
            this.totalCount = totalCount;
            this.persons = persons;
            this.activities = activities;
        }
    }

    public static class DetailResponse {
        public DealResponse deal;
        public List<PersonDTO> persons; // Persons linked to this deal
        public List<ActivityDTO> activities; // Activities linked to this deal

        public DetailResponse(DealResponse deal, List<PersonDTO> persons, List<ActivityDTO> activities) {
            this.deal = deal;
            this.persons = persons;
            this.activities = activities;
        }
    }

    public static class StageTotal {
        public Long stageId;
        public String stageName;
        public Long dealCount;
        public java.math.BigDecimal totalValue;

        public StageTotal(Long stageId, String stageName, Long dealCount, java.math.BigDecimal totalValue) {
            this.stageId = stageId;
            this.stageName = stageName;
            this.dealCount = dealCount;
            this.totalValue = totalValue;
        }
    }

    public static class UnassignedTotal {
        public Long dealCount;
        public java.math.BigDecimal totalValue;

        public UnassignedTotal(Long dealCount, java.math.BigDecimal totalValue) {
            this.dealCount = dealCount;
            this.totalValue = totalValue;
        }
    }

    public static class StageTotalsResponse {
        public List<StageTotal> stageTotals;
        public UnassignedTotal unassigned;

        public StageTotalsResponse(List<StageTotal> stageTotals, UnassignedTotal unassigned) {
            this.stageTotals = stageTotals;
            this.unassigned = unassigned;
        }
    }
}


