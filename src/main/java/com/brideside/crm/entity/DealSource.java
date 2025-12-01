package com.brideside.crm.entity;

import com.fasterxml.jackson.annotation.JsonCreator;

public enum DealSource {
    DIRECT,
    DIVERT,
    REFERENCE,
    PLANNER;

    /**
     * Converts a string value to DealSource enum, handling case variations.
     * Used by Jackson for JSON deserialization.
     */
    @JsonCreator
    public static DealSource fromString(String value) {
        if (value == null || value.trim().isEmpty()) {
            return null;
        }
        String normalized = value.trim().toUpperCase();
        try {
            return DealSource.valueOf(normalized);
        } catch (IllegalArgumentException e) {
            return null;
        }
    }

    /**
     * Converts enum to display string with proper capitalization.
     */
    public String toDisplayString() {
        switch (this) {
            case DIRECT:
                return "Direct";
            case DIVERT:
                return "Divert";
            case REFERENCE:
                return "Reference";
            case PLANNER:
                return "Planner";
            default:
                return this.name();
        }
    }

    /**
     * Returns the display name (alias for toDisplayString for consistency with other enums).
     */
    public String getDisplayName() {
        return toDisplayString();
    }
}

