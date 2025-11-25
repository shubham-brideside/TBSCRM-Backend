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
        
        // Handle old enum values by converting them to new values
        switch (normalized) {
            case "INSTAGRAM":
            case "WHATSAPP":
            case "EMAIL":
            case "CALL":
            case "WEBSITE":
                // Old values - convert to DIRECT
                return DealSource.DIRECT;
            case "DIRECT":
            case "DIVERT":
            case "REFERENCE":
            case "PLANNER":
                // New values - use as is
                try {
                    return DealSource.valueOf(normalized);
                } catch (IllegalArgumentException e) {
                    return null;
                }
            default:
                // Unknown value - return null
                return null;
        }
    }
}

