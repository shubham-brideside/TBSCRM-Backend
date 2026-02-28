package com.brideside.crm.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.math.BigDecimal;
import java.util.List;
import com.brideside.crm.entity.CreatedByType;
import com.brideside.crm.entity.DealStatus;

public class DealDtos {

    /**
     * Aggregated deal totals per sales user (organization owner).
     */
    public static class UserDealTotals {
        public Long userId;
        public String userName;
        public Long wonCount;
        public java.math.BigDecimal wonValue;
        public Long lostCount;
        public java.math.BigDecimal lostValue;
        public Long totalCount;
        public java.math.BigDecimal totalValue;

        public UserDealTotals(Long userId,
                              String userName,
                              Long wonCount,
                              java.math.BigDecimal wonValue,
                              Long lostCount,
                              java.math.BigDecimal lostValue,
                              Long totalCount,
                              java.math.BigDecimal totalValue) {
            this.userId = userId;
            this.userName = userName;
            this.wonCount = wonCount;
            this.wonValue = wonValue;
            this.lostCount = lostCount;
            this.lostValue = lostValue;
            this.totalCount = totalCount;
            this.totalValue = totalValue;
        }
    }

    public static class UserDealTotalsResponse {
        public java.util.List<UserDealTotals> users;

        public UserDealTotalsResponse(java.util.List<UserDealTotals> users) {
            this.users = users;
        }
    }

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
        public String notes; // optional: free-form notes about the deal
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
        public String notes; // optional: free-form notes about the deal
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
        /** optional: When the deal was marked WON (ISO-8601 date-time e.g. 2026-01-15T10:30:00, or date 2026-01-15). Only meaningful for WON deals. */
        @JsonProperty("won_at")
        public String wonAt;
        /** optional: When the deal was marked LOST (ISO-8601 date-time e.g. 2026-01-15T10:30:00, or date 2026-01-15). Only meaningful for LOST deals. */
        @JsonProperty("lost_at")
        public String lostAt;
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

    /**
     * Average time (in days) deals spend in each stage, grouped by stage name.
     */
    public static class AverageDealTimelineItem {
        public String stageName;
        public Double avgDaysInStage;
        public Long visitCount;

        public AverageDealTimelineItem(String stageName, Double avgDaysInStage, Long visitCount) {
            this.stageName = stageName;
            this.avgDaysInStage = avgDaysInStage;
            this.visitCount = visitCount;
        }
    }

    public static class AverageDealTimelineResponse {
        public List<AverageDealTimelineItem> byStageName;

        public AverageDealTimelineResponse(List<AverageDealTimelineItem> byStageName) {
            this.byStageName = byStageName;
        }
    }

    /**
     * Average deal timeline for one month (year-month when deals exited the stage).
     */
    public static class AverageDealTimelinePerMonth {
        public String month;
        public List<AverageDealTimelineItem> byStageName;

        public AverageDealTimelinePerMonth(String month, List<AverageDealTimelineItem> byStageName) {
            this.month = month;
            this.byStageName = byStageName;
        }
    }

    public static class AverageDealTimelinePerMonthResponse {
        public List<AverageDealTimelinePerMonth> byMonth;

        public AverageDealTimelinePerMonthResponse(List<AverageDealTimelinePerMonth> byMonth) {
            this.byMonth = byMonth;
        }
    }

    /**
     * Revenue calculation response
     */
    public static class RevenueResponse {
        public BigDecimal totalRevenue;
        public Long dealCount;
        public String currency; // Optional: for future use

        public RevenueResponse() {
        }

        public RevenueResponse(BigDecimal totalRevenue, Long dealCount) {
            this.totalRevenue = totalRevenue != null ? totalRevenue : BigDecimal.ZERO;
            this.dealCount = dealCount != null ? dealCount : 0L;
            this.currency = "INR"; // Default currency
        }

        public RevenueResponse(BigDecimal totalRevenue, Long dealCount, String currency) {
            this.totalRevenue = totalRevenue != null ? totalRevenue : BigDecimal.ZERO;
            this.dealCount = dealCount != null ? dealCount : 0L;
            this.currency = currency != null ? currency : "INR";
        }
    }
}


