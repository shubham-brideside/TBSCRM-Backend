package com.brideside.crm.dto;

import java.math.BigDecimal;

/**
 * DTOs for Category Manager dashboard (admin-style metrics scoped to their hierarchy).
 */
public class CategoryManagerDashboardDtos {

    /**
     * Summary stats for the Category Manager dashboard (category-level overview).
     */
    public static class SummaryResponse {
        /** Number of Sales managers (direct reports) under this Category Manager */
        public Long salesManagersCount;
        /** Number of Presales users under this Category Manager (direct + under Sales) */
        public Long presalesCount;
        /** Total team size (Sales + Presales under this Category Manager) */
        public Long totalTeamSize;
        /** Total WON deals (all time) in scope */
        public Long totalWonDeals;
        /** Total LOST deals (all time) in scope */
        public Long totalLostDeals;
        /** Total IN_PROGRESS deals in scope */
        public Long totalInProgressDeals;
        /** Total value of WON deals (all time) in scope */
        public BigDecimal totalWonValue;
        /** Total value of WON deals in current year (YTD) in scope */
        public BigDecimal totalWonValueYtd;
    }
}
