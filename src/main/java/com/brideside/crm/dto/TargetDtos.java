package com.brideside.crm.dto;

import com.brideside.crm.entity.TargetCategory;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

public class TargetDtos {

    public static class TargetUpsertRequest {
        public Long userId;
        public String category;
        public String periodType; // MONTHLY, QUARTERLY, HALF_YEARLY, YEARLY
        public Integer month; // For MONTHLY, QUARTERLY, HALF_YEARLY
        public Integer year;
        public Integer quarter; // For QUARTERLY (1-4)
        public Integer halfYear; // For HALF_YEARLY (1 or 2)
        public List<Long> organizationIds; // Multiple organizations (optional, combined target)
        public BigDecimal targetAmount;
    }

    public static class TargetResponse {
        public Long id;
        public Long userId;
        public String userName;
        public String category;
        public Integer month; // For MONTHLY, QUARTERLY, HALF_YEARLY
        public Integer year;
        public Integer quarter; // For QUARTERLY (1-4)
        public Integer halfYear; // For HALF_YEARLY (1 or 2)
        public List<Long> organizationIds; // Organizations linked to this target
        public List<OrganizationSummary> organizations; // Organization details
        public BigDecimal targetAmount;
        public boolean editable;
    }

    public static class OrganizationSummary {
        public Long id;
        public String name;
        public String category;
    }

    public static class TargetListFilter {
        public Integer month;
        public Integer year;
        public String category;
    }

    public static class DashboardFilter {
        public TargetCategory category;
        public TargetTimePreset preset;
        public Integer month;
        public Integer year;
        public Integer fromMonth;
        public Integer fromYear;
        public Integer toMonth;
        public Integer toYear;
    }

    public enum TargetTimePreset {
        THIS_MONTH("This month"),
        PREVIOUS_MONTH("Previous month"),
        NEXT_MONTH("Next month"),
        THIS_QUARTER("This quarter"),
        HALF_YEAR("Half yearly"),
        THIS_YEAR("This year"),
        CUSTOM_MONTH("Selected month"),
        CUSTOM_RANGE("Custom range");

        private final String label;

        TargetTimePreset(String label) {
            this.label = label;
        }

        public String getLabel() {
            return label;
        }

        public static TargetTimePreset fromValue(String raw) {
            if (raw == null || raw.isBlank()) {
                return null;
            }
            try {
                return TargetTimePreset.valueOf(raw.trim().toUpperCase());
            } catch (IllegalArgumentException ex) {
                return null;
            }
        }
    }

    public static class DashboardResponse {
        public AppliedFilters filters;
        public List<MonthBlock> months;
        public List<DealSummary> deals;
    }

    public static class AppliedFilters {
        public String timePreset;
        public Integer month;
        public Integer year;
        public Integer fromMonth;
        public Integer fromYear;
        public Integer toMonth;
        public Integer toYear;
        public String category;
        public boolean editableForCurrentUser;
    }

    public static class MonthBlock {
        public Integer month;
        public Integer year;
        public boolean editable;
        public List<CategoryTable> categories;
    }

    public static class CategoryTable {
        public String category;
        public String categoryLabel;
        public List<TargetRow> rows;
    }

    public static class CategoryMonthlyBreakdownResponse {
        public AppliedFilters filters;
        public String category;
        public String categoryLabel;
        public List<CategoryMonthlyRow> months;
        public CategoryMonthlyTotals totals;
    }

    public static class CategoryMonthlyRow {
        public Integer month;
        public Integer year;
        public BigDecimal totalTarget;
        public BigDecimal achieved;
        public BigDecimal achievementPercent;
        public Integer totalDeals;
        public BigDecimal incentivePercent;
        public BigDecimal incentiveAmount;
        public List<TargetRow> users;
    }

    public static class CategoryMonthlyTotals {
        public BigDecimal totalTarget;
        public BigDecimal achieved;
        public BigDecimal achievementPercent;
        public Integer totalDeals;
        public BigDecimal incentivePercent;
        public BigDecimal incentiveAmount;
    }

    public static class TargetRow {
        public Long userId;
        public String userName;
        public BigDecimal totalTarget;
        public BigDecimal achieved;
        public BigDecimal achievementPercent;
        public Integer totalDeals;
        public BigDecimal incentivePercent;
        public BigDecimal incentiveAmount;
    }

    public static class DealSummary {
        public Long dealId;
        public String dealName;
        public String instagramId;
        public BigDecimal dealValue;
        public BigDecimal commissionAmount;
        public String dealSource;
        public String personSource;
        public String phoneNumber;
        public String venue;
        public String eventDate;
        public String organization;
        public String category;
        public Long userId;
        public String userName;
    }

    public static class FiltersResponse {
        public List<FilterOption> categories;
        public List<FilterOption> presets;
        public int minYear;
    }

    public static class FilterOption {
        public String code;
        public String label;
    }

    public static class SalesUserWithOrganizations {
        public Long userId;
        public String userName;
        public String email;
        public List<OrganizationSummary> organizations;
    }

    public static class SalesUserOrganizationsResponse {
        public Long userId;
        public String userName;
        public String email;
        public List<OrganizationSummary> organizations;
        public Map<String, List<OrganizationSummary>> organizationsByMonth; // Key: "YYYY-MM"
    }
    
    public static class TargetUserMonthlyDetailResponse {
        public Long userId;
        public String userName;
        public Integer year;
        public List<String> categories; // legacy field, kept for backward compatibility
        public List<Long> organizationIds; // legacy field, kept for backward compatibility
        public List<OrganizationSummary> organizations; // legacy field, kept for backward compatibility
        public List<String> availableCategories; // Categories used in targets for this user/year
        public List<Long> availableOrganizationIds; // Organization IDs linked to targets
        public List<OrganizationSummary> availableOrganizations; // Organization details
        public List<UserMonthlyBreakdown> monthlyData;
    }
    
    public static class UserMonthlyBreakdown {
        public Integer month;
        public Integer year;
        public BigDecimal target;
        public BigDecimal achieved;
        public BigDecimal achievementPercent;
        public Integer totalDeals;
        public BigDecimal incentive;
        public Integer diversionDeals;
        public Integer instaDeals;
        public Integer referenceDeals;
        public Integer plannerDeals;
    }
}

