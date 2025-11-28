package com.brideside.crm.entity;

public enum DealLostReason {
    SLOT_NOT_OPENED("Slot not opened"),
    NOT_INTERESTED("Not Interested"),
    DATE_POSTPONED("Date postponed"),
    NOT_AVAILABLE("Not Available"),
    GHOSTED("Ghosted"),
    BUDGET("Budget"),
    BOOKED_SOMEONE_ELSE("Booked Someone else");

    private final String displayName;

    DealLostReason(String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }

    /**
     * Converts a string value to DealLostReason enum, handling case variations and spaces.
     * Accepts both enum name (e.g., "SLOT_NOT_OPENED") and display name (e.g., "Slot not opened").
     */
    public static DealLostReason fromString(String value) {
        if (value == null || value.trim().isEmpty()) {
            return null;
        }
        String normalized = value.trim();
        
        // First try exact enum name match (case-insensitive)
        try {
            return DealLostReason.valueOf(normalized.toUpperCase().replace(" ", "_"));
        } catch (IllegalArgumentException e) {
            // Try matching by display name
            for (DealLostReason reason : DealLostReason.values()) {
                if (reason.getDisplayName().equalsIgnoreCase(normalized)) {
                    return reason;
                }
            }
            return null;
        }
    }

    /**
     * Converts enum to display string (e.g., SLOT_NOT_OPENED -> "Slot not opened")
     */
    public String toDisplayString() {
        return this.displayName;
    }
}

