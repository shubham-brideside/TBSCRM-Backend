package com.brideside.crm.repository;

import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.DealStatus;
import org.springframework.data.jpa.domain.Specification;

import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

public final class DealSpecifications {

    private DealSpecifications() {
    }

    /**
     * Excludes soft-deleted deals from queries
     */
    public static Specification<Deal> notDeleted() {
        return (root, query, cb) -> cb.or(
            cb.isNull(root.get("isDeleted")),
            cb.equal(root.get("isDeleted"), false)
        );
    }

    /**
     * Filter by pipeline ID
     * Uses the foreign key column directly (pipeline_id) without joining the pipelines table
     */
    public static Specification<Deal> hasPipeline(Long pipelineId) {
        if (pipelineId == null) {
            return null;
        }
        return (root, query, cb) -> 
            cb.equal(root.get("pipelineId"), pipelineId);
    }

    /**
     * Filter by status
     * If status is 'all' or 'ALL', return null (no filter)
     * Case-insensitive matching
     */
    public static Specification<Deal> hasStatus(String status) {
        if (status == null || status.trim().isEmpty()) {
            return null;
        }
        String statusTrimmed = status.trim();
        // Handle 'all' case (case-insensitive)
        if ("all".equalsIgnoreCase(statusTrimmed)) {
            return null;
        }
        try {
            DealStatus dealStatus = DealStatus.valueOf(statusTrimmed.toUpperCase());
            return (root, query, cb) -> cb.equal(root.get("status"), dealStatus);
        } catch (IllegalArgumentException e) {
            // Invalid status, return null (no filter) - don't throw exception to allow other filters to work
            return null;
        }
    }

    /**
     * Filter by organization ID
     * Uses the foreign key column directly (organization_id) without joining the organizations table
     */
    public static Specification<Deal> hasOrganization(Long organizationId) {
        if (organizationId == null) {
            return null;
        }
        return (root, query, cb) -> 
            cb.equal(root.get("organizationId"), organizationId);
    }

    /**
     * Filter by category ID
     * Checks both deal.category_id and pipeline.category (where pipeline.category matches the category name)
     * Uses direct foreign key field for deal.category_id to avoid JOIN
     */
    public static Specification<Deal> hasCategory(Long categoryId) {
        if (categoryId == null) {
            return null;
        }
        return (root, query, cb) -> {
            // Check deal.category_id = categoryId (using direct foreign key field, no JOIN)
            jakarta.persistence.criteria.Predicate dealCategoryMatch = 
                cb.equal(root.get("categoryId"), categoryId);
            
            // Check pipeline.category = (category name from categoryId)
            // Use subquery to get category name
            jakarta.persistence.criteria.Subquery<String> categoryNameSubquery = 
                query.subquery(String.class);
            jakarta.persistence.criteria.Root<com.brideside.crm.entity.Category> categoryRoot = 
                categoryNameSubquery.from(com.brideside.crm.entity.Category.class);
            categoryNameSubquery.select(categoryRoot.get("name"))
                .where(cb.equal(categoryRoot.get("id"), categoryId));
            
            // Only join pipeline if we need to check pipeline.category
            Join<Object, Object> pipelineJoin = root.join("pipeline", JoinType.LEFT);
            jakarta.persistence.criteria.Predicate pipelineCategoryMatch = 
                cb.equal(pipelineJoin.get("category"), categoryNameSubquery);
            
            return cb.or(dealCategoryMatch, pipelineCategoryMatch);
        };
    }

    /**
     * Filter by manager ID (person's owner_id)
     */
    public static Specification<Deal> hasManager(Long managerId) {
        if (managerId == null) {
            return null;
        }
        return (root, query, cb) -> {
            Join<Object, Object> personJoin = root.join("person", JoinType.INNER);
            return cb.equal(personJoin.get("owner").get("id"), managerId);
        };
    }

    /**
     * Filter by date range (createdAt between dateFrom and dateTo, inclusive)
     * Filters deals where createdAt falls within the specified range
     */
    public static Specification<Deal> createdBetween(LocalDate dateFrom, LocalDate dateTo) {
        if (dateFrom == null && dateTo == null) {
            return null;
        }
        return (root, query, cb) -> {
            jakarta.persistence.criteria.Expression<LocalDateTime> createdAt = root.get("createdAt");
            
            if (dateFrom != null && dateTo != null) {
                // Both dates provided: between dateFrom 00:00:00 and dateTo 23:59:59.999
                LocalDateTime start = dateFrom.atStartOfDay();
                LocalDateTime end = dateTo.atTime(LocalTime.MAX);
                return cb.between(createdAt, start, end);
            } else if (dateFrom != null) {
                // Only dateFrom: >= dateFrom 00:00:00
                LocalDateTime start = dateFrom.atStartOfDay();
                return cb.greaterThanOrEqualTo(createdAt, start);
            } else {
                // Only dateTo: <= dateTo 23:59:59.999
                LocalDateTime end = dateTo.atTime(LocalTime.MAX);
                return cb.lessThanOrEqualTo(createdAt, end);
            }
        };
    }

    /**
     * Search across multiple fields: deal name, venue, person name, and organization name (case-insensitive)
     */
    public static Specification<Deal> search(String searchQuery) {
        if (searchQuery == null || searchQuery.trim().isEmpty()) {
            return null;
        }
        String likePattern = "%" + searchQuery.trim().toLowerCase() + "%";
        return (root, query, cb) -> {
            // Search in deal name and venue
            jakarta.persistence.criteria.Predicate dealNamePredicate = cb.like(cb.lower(root.get("name")), likePattern);
            jakarta.persistence.criteria.Predicate venuePredicate = cb.like(cb.lower(root.get("venue")), likePattern);
            
            // Search in person name (if person exists)
            Join<Object, Object> personJoin = root.join("person", JoinType.LEFT);
            jakarta.persistence.criteria.Predicate personNamePredicate = cb.like(
                cb.lower(personJoin.get("name")), likePattern
            );
            
            // Search in organization name (if organization exists)
            Join<Object, Object> orgJoin = root.join("organization", JoinType.LEFT);
            jakarta.persistence.criteria.Predicate orgNamePredicate = cb.like(
                cb.lower(orgJoin.get("name")), likePattern
            );
            
            return cb.or(dealNamePredicate, venuePredicate, personNamePredicate, orgNamePredicate);
        };
    }

    /**
     * Filter by deal source
     */
    public static Specification<Deal> hasSource(com.brideside.crm.entity.DealSource source) {
        if (source == null) {
            return null;
        }
        return (root, query, cb) -> cb.equal(root.get("dealSource"), source);
    }

    /**
     * Filter by stage ID
     * Uses the foreign key column directly (stage_id) without joining the stages table
     */
    public static Specification<Deal> hasStage(Long stageId) {
        if (stageId == null) {
            return null;
        }
        return (root, query, cb) -> 
            cb.equal(root.get("stageId"), stageId);
    }
}

