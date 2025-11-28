package com.brideside.crm.entity;

public enum DealLabel {
    DIRECT,
    DIVERT,
    DESTINATION,
    PARTY_MAKEUP,
    PRE_WEDDING;

    /**
     * Converts a string value to DealLabel enum, handling spaces and case variations.
     * "PARTY MAKEUP" -> PARTY_MAKEUP
     */
    public static DealLabel fromString(String value) {
        if (value == null || value.trim().isEmpty()) {
            return null;
        }
        String normalized = value.trim().toUpperCase().replace(" ", "_");
        try {
            return DealLabel.valueOf(normalized);
        } catch (IllegalArgumentException e) {
            return null;
        }
    }

    /**
     * Converts enum to display string (e.g., PARTY_MAKEUP -> "PARTY MAKEUP")
     */
    public String toDisplayString() {
        return this.name().replace("_", " ");
    }
}

