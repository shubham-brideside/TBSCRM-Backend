package com.brideside.crm.dto;

import java.math.BigDecimal;
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
        public Boolean finalThankYouSent; // optional
        public Boolean eventDateAsked; // optional
        public Boolean contactNumberAsked; // optional
        public Boolean venueAsked; // optional
        public String eventDate; // ISO-8601 date string (yyyy-MM-dd)
        public String label; // optional: DIRECT, DIVERT, DESTINATION, PARTY MAKEUP, PRE WEDDING
        public String source; // optional: Instagram, Whatsapp, Email, Reference, Call, Website
        public Long referencedDealId; // optional: ID of the original deal when diverting (required when label is DIVERT)
    }

    public static class UpdateStageRequest {
        public Long stageId;
    }

    public static class MarkStatusRequest {
        public DealStatus status;
        public String lostReason; // optional: Required when status is LOST. Values: "Slot not opened", "Not Interested", "Date postponed", "Not Available", "Ghosted", "Budget", "Booked Someone else"
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
        public Boolean finalThankYouSent;
        public Boolean eventDateAsked;
        public Boolean contactNumberAsked;
        public Boolean venueAsked;
        public String eventDate; // ISO-8601 date string (yyyy-MM-dd)
        public String label; // optional: DIRECT, DIVERT, DESTINATION, PARTY MAKEUP, PRE WEDDING
        public String source; // optional: Instagram, Whatsapp, Email, Reference, Call, Website
    }
}


