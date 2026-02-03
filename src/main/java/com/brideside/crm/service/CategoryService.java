package com.brideside.crm.service;

import com.brideside.crm.dto.CategoryDtos;
import com.brideside.crm.dto.PipelineDtos;

import java.util.List;

public interface CategoryService {
    /**
     * Get all managers related to a category.
     * Managers include:
     * - Team managers of pipelines with the specified category
     * - Person owners of deals with the specified category
     * 
     * @param categoryIdentifier Category ID (Long) or category name (String)
     * @return List of managers (users with SALES or MANAGER role)
     */
    List<CategoryDtos.ManagerResponse> getManagersByCategory(String categoryIdentifier);
    
    /**
     * Get all pipelines related to a category.
     * Returns pipelines where pipeline.category matches the specified category,
     * filtered by user permissions.
     * 
     * @param categoryIdentifier Category ID (Long) or category name (String)
     * @param includeStages Whether to include stages in the response
     * @return List of pipelines
     */
    List<PipelineDtos.PipelineResponse> getPipelinesByCategory(String categoryIdentifier, boolean includeStages);
}

