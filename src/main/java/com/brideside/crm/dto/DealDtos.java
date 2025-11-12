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
    }

    public static class UpdateStageRequest {
        public Long stageId;
    }

    public static class MarkStatusRequest {
        public DealStatus status;
    }
}


