package com.brideside.crm.repository;

import com.brideside.crm.entity.Label;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface LabelRepository extends JpaRepository<Label, Long> {

    /**
     * Find all labels for a specific organization
     */
    List<Label> findByOrganizationId(Long organizationId);

    /**
     * Find all labels for a specific pipeline
     */
    List<Label> findByPipelineId(Long pipelineId);

    /**
     * Find all global labels
     */
    List<Label> findByIsGlobalTrue();

    /**
     * Find labels by organization ID or global labels
     */
    @Query("SELECT l FROM Label l WHERE l.organizationId = :organizationId OR l.isGlobal = true ORDER BY l.name")
    List<Label> findByOrganizationIdOrGlobal(@Param("organizationId") Long organizationId);

    /**
     * Find labels by pipeline ID or global labels
     */
    @Query("SELECT l FROM Label l WHERE l.pipelineId = :pipelineId OR l.isGlobal = true ORDER BY l.name")
    List<Label> findByPipelineIdOrGlobal(@Param("pipelineId") Long pipelineId);

    /**
     * Find labels by name (case-insensitive)
     */
    List<Label> findByNameIgnoreCase(String name);

    /**
     * Find labels created by a specific user
     */
    List<Label> findByCreatedByUserId(Long userId);

    /**
     * Check if a label with the given name exists for an organization
     */
    boolean existsByNameIgnoreCaseAndOrganizationId(String name, Long organizationId);

    /**
     * Check if a label with the given name exists for a pipeline
     */
    boolean existsByNameIgnoreCaseAndPipelineId(String name, Long pipelineId);
}
