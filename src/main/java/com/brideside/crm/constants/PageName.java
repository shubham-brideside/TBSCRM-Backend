package com.brideside.crm.constants;

import java.util.Arrays;
import java.util.List;

/**
 * Constants for page names used in page access management.
 * These should match the route names used in the frontend.
 */
public class PageName {
    // Main pages
    public static final String PERSONS = "persons";
    public static final String DEALS = "deals";
    public static final String CALENDAR = "calendar";
    public static final String TEAMS = "teams";
    public static final String ACTIVITIES = "activities";
    public static final String ORGANIZATIONS = "organizations";
    public static final String USERS = "users";
    public static final String TARGETS = "targets";
    public static final String PIPELINES = "pipelines";
    
    // Dashboard pages
    public static final String DASHBOARD_SALES = "dashboard_sales";
    public static final String DASHBOARD_CATEGORY_MANAGER = "dashboard_category_manager";
    public static final String DASHBOARD_PRE_SALES = "dashboard_pre_sales";
    
    // Report pages
    public static final String REPORTS_DEAL_SOURCE = "reports_deal_source";
    public static final String REPORTS_DEAL_SUB_SOURCE = "reports_deal_sub_source";
    public static final String REPORTS_DEAL_STATUS = "reports_deal_status";
    public static final String REPORTS_DEAL_LOST_REASON = "reports_deal_lost_reason";
    public static final String REPORTS_DEAL_DURATION = "reports_deal_duration";
    
    // Admin page (always accessible to admins)
    public static final String PAGE_ACCESS_MANAGEMENT = "page_access_management";
    
    /**
     * Get all valid page names
     */
    public static List<String> getAllPageNames() {
        return Arrays.asList(
            PERSONS, DEALS, CALENDAR, TEAMS, ACTIVITIES, ORGANIZATIONS, 
            USERS, TARGETS, PIPELINES,
            DASHBOARD_SALES, DASHBOARD_CATEGORY_MANAGER, DASHBOARD_PRE_SALES,
            REPORTS_DEAL_SOURCE, REPORTS_DEAL_SUB_SOURCE, REPORTS_DEAL_STATUS,
            REPORTS_DEAL_LOST_REASON, REPORTS_DEAL_DURATION,
            PAGE_ACCESS_MANAGEMENT
        );
    }
    
    /**
     * Check if a page name is valid
     */
    public static boolean isValid(String pageName) {
        return getAllPageNames().contains(pageName);
    }
}

