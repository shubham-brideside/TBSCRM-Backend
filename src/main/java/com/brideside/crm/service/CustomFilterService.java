package com.brideside.crm.service;

import com.brideside.crm.dto.CustomFilterDtos;

import java.util.List;
import java.util.Map;

public interface CustomFilterService {
    Map<String, List<CustomFilterDtos.FilterCondition>> getAllFilters(Long userId, String entityType);
    CustomFilterDtos.FilterResponse saveFilter(Long userId, String entityType, CustomFilterDtos.SaveFilterRequest request);
    void deleteFilter(Long userId, String entityType, String filterName);
}
