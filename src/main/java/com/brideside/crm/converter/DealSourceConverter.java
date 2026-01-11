package com.brideside.crm.converter;

import com.brideside.crm.entity.DealSource;
import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;

@Converter(autoApply = true)
public class DealSourceConverter implements AttributeConverter<DealSource, String> {

    @Override
    public String convertToDatabaseColumn(DealSource attribute) {
        if (attribute == null) {
            return null;
        }
        return attribute.name();
    }

    @Override
    public DealSource convertToEntityAttribute(String dbData) {
        if (dbData == null || dbData.trim().isEmpty()) {
            return null;
        }
        
        String normalized = dbData.trim().toUpperCase();
        
        // Handle old PersonSource enum values by converting them to new DealSource values
        switch (normalized) {
            case "INSTAGRAM":
            case "WHATSAPP":
            case "EMAIL":
            case "CALL":
            case "WEBSITE":
            case "TBS_WEBSITE":
                // Old PersonSource values - convert to DIRECT
                return DealSource.DIRECT;
            case "REFERRAL":
                // Old PersonSource REFERRAL - convert to REFERENCE
                return DealSource.REFERENCE;
            case "OTHER":
                // Old PersonSource OTHER - convert to DIRECT (or could be null)
                return DealSource.DIRECT;
            case "DIRECT":
            case "DIVERT":
            case "REFERENCE":
            case "PLANNER":
            case "TBS":
                // New DealSource values - use as is
                try {
                    return DealSource.valueOf(normalized);
                } catch (IllegalArgumentException e) {
                    return null;
                }
            default:
                // Unknown value - try using fromString method, return null if fails
                return DealSource.fromString(dbData);
        }
    }
}

