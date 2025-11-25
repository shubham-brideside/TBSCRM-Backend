package com.brideside.crm.entity;

public enum DealSubSource {
    INSTAGRAM("Instagram"),
    WHATSAPP("Whatsapp"),
    LANDING_PAGE("Landing Page"),
    EMAIL("Email");

    private final String displayName;

    DealSubSource(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }

    /**
     * Converts a string value to DealSubSource enum, handling case variations and spaces.
     * Accepts both enum name (e.g., "LANDING_PAGE") and display name (e.g., "Landing Page").
     */
    public static DealSubSource fromString(String value) {
        if (value == null || value.trim().isEmpty()) {
            return null;
        }
        String normalized = value.trim();
        
        // First try exact enum name match (case-insensitive, handle spaces/underscores)
        try {
            String enumName = normalized.toUpperCase().replace(" ", "_");
            return DealSubSource.valueOf(enumName);
        } catch (IllegalArgumentException e) {
            // Try matching by display name
            for (DealSubSource subSource : DealSubSource.values()) {
                if (subSource.getDisplayName().equalsIgnoreCase(normalized)) {
                    return subSource;
                }
            }
            return null;
        }
    }

    /**
     * Converts enum to display string (e.g., LANDING_PAGE -> "Landing Page")
     */
    public String toDisplayString() {
        return this.displayName;
    }
}

