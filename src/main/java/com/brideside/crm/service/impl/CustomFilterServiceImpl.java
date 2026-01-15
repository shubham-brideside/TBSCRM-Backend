package com.brideside.crm.service.impl;

import com.brideside.crm.dto.CustomFilterDtos;
import com.brideside.crm.entity.CustomFilter;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.CustomFilterRepository;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.CustomFilterService;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class CustomFilterServiceImpl implements CustomFilterService {

    private static final int MAX_FILTER_NAME_LENGTH = 255;
    private static final int MAX_CONDITIONS_PER_FILTER = 20;
    private static final int MAX_FILTERS_PER_USER = 50;

    @Autowired
    private CustomFilterRepository customFilterRepository;

    @Autowired
    private UserRepository userRepository;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    @Transactional(readOnly = true)
    public Map<String, List<CustomFilterDtos.FilterCondition>> getAllFilters(Long userId, String entityType) {
        List<CustomFilter> filters = customFilterRepository.findByUserIdAndEntityType(userId, entityType);
        
        Map<String, List<CustomFilterDtos.FilterCondition>> result = new HashMap<>();
        
        for (CustomFilter filter : filters) {
            try {
                List<CustomFilterDtos.FilterCondition> conditions = objectMapper.readValue(
                    filter.getConditions(),
                    new TypeReference<List<CustomFilterDtos.FilterCondition>>() {}
                );
                result.put(filter.getFilterName(), conditions);
            } catch (Exception e) {
                // Skip filters with invalid JSON
                continue;
            }
        }
        
        return result;
    }

    @Override
    @Transactional
    public CustomFilterDtos.FilterResponse saveFilter(Long userId, String entityType, CustomFilterDtos.SaveFilterRequest request) {
        // Validate user exists
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with id: " + userId));

        // Validate entity type
        if (entityType == null || entityType.trim().isEmpty()) {
            throw new BadRequestException("Entity type is required");
        }

        // Validate request
        if (request.getName() == null || request.getName().trim().isEmpty()) {
            throw new BadRequestException("Filter name is required");
        }

        String filterName = request.getName().trim();
        
        // Validate filter name length
        if (filterName.length() > MAX_FILTER_NAME_LENGTH) {
            throw new BadRequestException("Filter name must not exceed " + MAX_FILTER_NAME_LENGTH + " characters");
        }

        // Validate conditions
        if (request.getConditions() == null || request.getConditions().isEmpty()) {
            throw new BadRequestException("At least one condition is required");
        }

        if (request.getConditions().size() > MAX_CONDITIONS_PER_FILTER) {
            throw new BadRequestException("Maximum " + MAX_CONDITIONS_PER_FILTER + " conditions allowed per filter");
        }

        // Check user's filter count for this entity type
        long userFilterCount = customFilterRepository.findByUserIdAndEntityType(userId, entityType).size();
        if (userFilterCount >= MAX_FILTERS_PER_USER) {
            throw new BadRequestException("Maximum " + MAX_FILTERS_PER_USER + " filters allowed per user for " + entityType);
        }

        // Validate each condition
        for (CustomFilterDtos.FilterCondition condition : request.getConditions()) {
            validateCondition(condition, entityType);
        }

        // Check for duplicate filter name for this entity type
        boolean exists = customFilterRepository.existsByUserIdAndFilterNameAndEntityType(userId, filterName, entityType);
        
        if (exists) {
            throw new BadRequestException("Filter with this name already exists for " + entityType);
        }
        
        // Create new filter
        CustomFilter filter = new CustomFilter();
        filter.setUser(user);
        filter.setFilterName(filterName);
        filter.setEntityType(entityType);

        // Convert conditions to JSON
        try {
            String conditionsJson = objectMapper.writeValueAsString(request.getConditions());
            filter.setConditions(conditionsJson);
        } catch (Exception e) {
            throw new BadRequestException("Failed to serialize filter conditions: " + e.getMessage());
        }

        // Save filter
        CustomFilter saved = customFilterRepository.save(filter);

        // Return response
        CustomFilterDtos.FilterResponse response = new CustomFilterDtos.FilterResponse();
        response.setName(saved.getFilterName());
        response.setConditions(request.getConditions());
        return response;
    }

    @Override
    @Transactional
    public void deleteFilter(Long userId, String entityType, String filterName) {
        // Validate user exists
        userRepository.findById(userId)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with id: " + userId));

        // Check if filter exists and belongs to user for this entity type
        CustomFilter filter = customFilterRepository.findByUserIdAndFilterNameAndEntityType(userId, filterName, entityType)
                .orElseThrow(() -> new ResourceNotFoundException("Filter not found"));

        // Delete filter
        customFilterRepository.delete(filter);
    }

    /**
     * Validates a filter condition
     */
    private void validateCondition(CustomFilterDtos.FilterCondition condition, String entityType) {
        if (condition.getField() == null || condition.getField().trim().isEmpty()) {
            throw new BadRequestException("Condition field is required");
        }

        if (condition.getOperator() == null || condition.getOperator().trim().isEmpty()) {
            throw new BadRequestException("Condition operator is required");
        }

        if (condition.getValue() == null) {
            throw new BadRequestException("Condition value is required");
        }

        // Validate date format for date fields only
        String field = condition.getField();
        if (isDateField(field, entityType)) {
            validateDateFormat(field, condition.getValue(), entityType);
        }
    }

    /**
     * Checks if a field is a date field based on entity type
     * Only validates actual date fields, not all fields with prefixes
     */
    private boolean isDateField(String field, String entityType) {
        if (field == null || field.trim().isEmpty()) {
            return false;
        }
        
        String normalizedField = field.trim();
        
        // Person entity date fields
        if ("persons".equals(entityType)) {
            return normalizedField.equals("leadDate") ||
                   normalizedField.equals("person.leadDate") ||
                   normalizedField.equals("person.eventDate");
        }
        
        // Deal entity date fields - only actual date fields
        if ("deals".equals(entityType)) {
            return normalizedField.equals("deal.eventDate") ||
                   normalizedField.equals("deal.createdAt") ||
                   normalizedField.equals("deal.updatedAt") ||
                   normalizedField.equals("deal.lostTime") ||
                   normalizedField.equals("deal.wonTime") ||
                   normalizedField.equals("person.leadDate") ||
                   normalizedField.equals("person.eventDate") ||
                   normalizedField.equals("activity.dueDate") ||
                   normalizedField.equals("activity.date");
        }
        
        // Activities entity date fields (if needed in future)
        if ("activities".equals(entityType)) {
            return normalizedField.equals("dueDate") ||
                   normalizedField.equals("date") ||
                   normalizedField.equals("activity.dueDate") ||
                   normalizedField.equals("activity.date");
        }
        
        return false;
    }

    /**
     * Validates date format - accepts both DD/MM/YYYY and YYYY-MM-DD formats
     * For deals entity, prefers YYYY-MM-DD format
     * For persons entity, accepts both formats
     */
    private void validateDateFormat(String field, String value, String entityType) {
        // Try YYYY-MM-DD format first (preferred for Deal pages)
        if (value.matches("^\\d{4}-\\d{2}-\\d{2}$")) {
            try {
                LocalDate.parse(value, DateTimeFormatter.ofPattern("yyyy-MM-dd"));
                return; // Valid date in YYYY-MM-DD format
            } catch (DateTimeParseException e) {
                // Continue to try other formats
            }
        }
        
        // Try DD/MM/YYYY format (preferred for Person pages)
        if (value.matches("^\\d{2}/\\d{2}/\\d{4}$")) {
            try {
                LocalDate.parse(value, DateTimeFormatter.ofPattern("dd/MM/yyyy"));
                return; // Valid date in DD/MM/YYYY format
            } catch (DateTimeParseException e) {
                // Continue to throw error
            }
        }
        
        // Neither format matched or both failed to parse
        String formatHint = "deals".equals(entityType) 
            ? "YYYY-MM-DD format (e.g., 2026-01-15)"
            : "DD/MM/YYYY format (e.g., 15/01/2026) or YYYY-MM-DD format (e.g., 2026-01-15)";
        
        throw new BadRequestException(
            "Date field '" + field + "' must use " + formatHint
        );
    }
}
