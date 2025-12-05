package com.brideside.crm.dto;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import com.brideside.crm.entity.CreatedByType;
import com.brideside.crm.entity.DealStatus;

public class DealResponse {
    public Long id;
    public String name;
    public BigDecimal value;
    public Long personId;
    public String personName; // Person name for display/tooltip
    public Long pipelineId;
    public Long stageId;
    public Long sourceId;
    public Long organizationId;
    public String organizationName; // Organization name for display/tooltip
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
    public String eventDate; // ISO-8601 date - legacy, use eventDates instead
    public List<String> eventDates; // List of ISO-8601 date strings (yyyy-MM-dd)
    public String label; // DIRECT, DIVERT, DESTINATION, PARTY MAKEUP, PRE WEDDING
    public String source; // Direct, Divert, Reference, Planner
    public String subSource; // Instagram, Whatsapp, Landing Page, Email (only present when source is "Direct")
    public Boolean isDiverted; // true if this deal was diverted from another deal
    public Long referencedDealId; // ID of the original deal if this is a diverted deal
    public Long referencedPipelineId; // ID of the pipeline from which the deal was diverted
    public Long sourcePipelineId; // ID of the initial/source pipeline where the deal was first created
    public String pipelineHistory; // JSON array of pipeline IDs the deal has been in: [1, 2, 3]
    public Boolean isDeleted; // true if this deal has been soft deleted
    public String lostReason; // Reason why deal was marked as LOST: "Slot not opened", "Not Interested", "Date postponed", "Not Available", "Ghosted", "Budget", "Booked Someone else"
    public CreatedByType createdBy; // USER or BOT - who created the deal
    public Long createdByUserId; // User ID if created by USER
    public String createdByName; // User name if created by USER
}



