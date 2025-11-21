package com.brideside.crm.dto;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import com.brideside.crm.entity.DealStatus;

public class DealResponse {
    public Long id;
    public String name;
    public BigDecimal value;
    public Long personId;
    public Long pipelineId;
    public Long stageId;
    public Long sourceId;
    public Long organizationId;
    public Long categoryId;
    public String eventType;
    public DealStatus status;
    public BigDecimal commissionAmount;
    public LocalDateTime createdAt;
    public String venue;
    public String phoneNumber;
    public Boolean finalThankYouSent;
    public Boolean eventDateAsked;
    public Boolean contactNumberAsked;
    public Boolean venueAsked;
    public String eventDate; // ISO-8601 date
    public String googleCalendarEventId;
}



