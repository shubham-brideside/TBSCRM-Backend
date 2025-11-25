package com.brideside.crm.entity;

import java.util.Arrays;
import java.util.Locale;
import java.util.stream.Stream;

/**
 * Canonical list of categories used by the Target dashboard. The CRM currently
 * works with three business units – Photography, Makeup, and Planning & Decor.
 * This enum centralizes their codes plus a handful of legacy aliases so we can
 * safely reconcile values coming from various tables (deals, organizations, etc).
 */
public enum TargetCategory {
    PHOTOGRAPHY("Photography", new String[]{"PHOTOGRAPHY", "PHOTO", "PHOTOGRAPHERS"}),
    MAKEUP("Makeup", new String[]{"MAKEUP", "MUA"}),
    PLANNING_AND_DECOR("Planning & Decor",
            new String[]{"PLANNING_AND_DECOR", "PLANNING & DECOR", "PLANNING AND DECOR",
                    "PLANNING_DECOR", "PLANNING & DECORATION", "PLANNING AND DECORATION", "DECOR"});

    private final String label;
    private final String[] aliases;

    TargetCategory(String label, String[] aliases) {
        this.label = label;
        this.aliases = aliases;
    }

    public String getLabel() {
        return label;
    }

    public String getCode() {
        return name();
    }

    /**
     * Resolves the input string (DB value, enum name, user-entered label, etc.) to
     * one of the supported categories. Returns null when no mapping is found.
     */
    public static TargetCategory fromValue(String raw) {
        if (raw == null || raw.isBlank()) {
            return null;
        }
        String normalized = normalize(raw);
        return Stream.of(values())
                .filter(cat -> normalize(cat.name()).equals(normalized)
                        || Arrays.stream(cat.aliases).map(TargetCategory::normalize).anyMatch(normalized::equals))
                .findFirst()
                .orElse(null);
    }

    private static String normalize(String value) {
        if (value == null) {
            return "";
        }
        return value
                .replace("&", "AND")
                .replace("-", " ")
                .replace("_", " ")
                .replace("/", " ")
                .trim()
                .replaceAll("\\s+", " ")
                .toUpperCase(Locale.ROOT);
    }

    /**
     * Try to infer the target category for the provided deal, looking at the
     * deal's category entity, legacy category text, organization category and
     * pipeline category fields. Returns null if the deal cannot be classified.
     */
    public static TargetCategory fromDeal(Deal deal) {
        if (deal == null) {
            return null;
        }

        if (deal.getDealCategory() != null && deal.getDealCategory().getName() != null) {
            TargetCategory cat = fromValue(deal.getDealCategory().getName());
            if (cat != null) return cat;
        }

        if (deal.getCategory() != null) {
            TargetCategory cat = fromValue(deal.getCategory());
            if (cat != null) return cat;
        }

        if (deal.getOrganization() != null && deal.getOrganization().getCategory() != null) {
            TargetCategory cat = fromValue(deal.getOrganization().getCategory().getDbValue());
            if (cat != null) return cat;
        }

        if (deal.getPipeline() != null && deal.getPipeline().getCategory() != null) {
            TargetCategory cat = fromValue(deal.getPipeline().getCategory());
            if (cat != null) return cat;
        }

        return null;
    }
}

