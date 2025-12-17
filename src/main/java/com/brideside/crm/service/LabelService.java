package com.brideside.crm.service;

import com.brideside.crm.dto.LabelDtos.CreateLabelRequest;
import com.brideside.crm.dto.LabelDtos.UpdateLabelRequest;
import com.brideside.crm.entity.Label;

import java.util.List;
import java.util.Optional;

public interface LabelService {

    /**
     * Create a new label
     */
    Label create(CreateLabelRequest request);

    /**
     * Get all labels
     */
    List<Label> findAll();

    /**
     * Get label by ID
     */
    Optional<Label> findById(Long id);

    /**
     * Get labels by organization ID (includes global labels)
     */
    List<Label> findByOrganizationId(Long organizationId);

    /**
     * Get labels by pipeline ID (includes global labels)
     */
    List<Label> findByPipelineId(Long pipelineId);

    /**
     * Get all global labels
     */
    List<Label> findGlobalLabels();

    /**
     * Update an existing label
     */
    Label update(Long id, UpdateLabelRequest request);

    /**
     * Delete a label by ID
     */
    void delete(Long id);

    /**
     * Check if a label exists
     */
    boolean existsById(Long id);
}
