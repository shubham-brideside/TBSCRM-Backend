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
     */
    public static Specification<Deal> hasPipeline(Long pipelineId) {
        if (pipelineId == null) {
            return null;
        }
        return (root, query, cb) -> 
            cb.equal(root.get("pipeline").get("id"), pipelineId);
    }

    /**
     * Filter by status
     * If status is 'all', return null (no filter)
     */
    public static Specification<Deal> hasStatus(String status) {
        if (status == null || status.trim().isEmpty() || "all".equalsIgnoreCase(status.trim())) {
            return null;
        }
        try {
            DealStatus dealStatus = DealStatus.valueOf(status.trim().toUpperCase());
            return (root, query, cb) -> cb.equal(root.get("status"), dealStatus);
        } catch (IllegalArgumentException e) {
            // Invalid status, return null (no filter)
            return null;
        }
    }

    /**
     * Filter by organization ID
     */
    public static Specification<Deal> hasOrganization(Long organizationId) {
        if (organizationId == null) {
            return null;
        }
        return (root, query, cb) -> 
            cb.equal(root.get("organization").get("id"), organizationId);
    }

    /**
     * Filter by category ID
     * Checks both deal.category_id and pipeline.category (where pipeline.category matches the category name)
     */
    public static Specification<Deal> hasCategory(Long categoryId) {
        if (categoryId == null) {
            return null;
        }
        return (root, query, cb) -> {
            // Check deal.category_id = categoryId
            jakarta.persistence.criteria.Predicate dealCategoryMatch = 
                cb.equal(root.get("dealCategory").get("id"), categoryId);
            
            // Check pipeline.category = (category name from categoryId)
            // Use subquery to get category name
            jakarta.persistence.criteria.Subquery<String> categoryNameSubquery = 
                query.subquery(String.class);
            jakarta.persistence.criteria.Root<com.brideside.crm.entity.Category> categoryRoot = 
                categoryNameSubquery.from(com.brideside.crm.entity.Category.class);
            categoryNameSubquery.select(categoryRoot.get("name"))
                .where(cb.equal(categoryRoot.get("id"), categoryId));
            
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
     * Filter by date range (created_at between dateFrom and dateTo, inclusive)
     */
    public static Specification<Deal> createdBetween(LocalDate dateFrom, LocalDate dateTo) {
        if (dateFrom == null && dateTo == null) {
            return null;
        }
        return (root, query, cb) -> {
            if (dateFrom != null && dateTo != null) {
                // Both dates provided: between dateFrom 00:00:00 and dateTo 23:59:59.999
                LocalDateTime start = dateFrom.atStartOfDay();
                LocalDateTime end = dateTo.atTime(LocalTime.MAX);
                return cb.between(root.get("createdAt"), start, end);
            } else if (dateFrom != null) {
                // Only dateFrom: >= dateFrom 00:00:00
                LocalDateTime start = dateFrom.atStartOfDay();
                return cb.greaterThanOrEqualTo(root.get("createdAt"), start);
            } else {
                // Only dateTo: <= dateTo 23:59:59.999
                LocalDateTime end = dateTo.atTime(LocalTime.MAX);
                return cb.lessThanOrEqualTo(root.get("createdAt"), end);
            }
        };
    }

    /**
     * Search in deal name and venue (case-insensitive)
     */
    public static Specification<Deal> search(String searchQuery) {
        if (searchQuery == null || searchQuery.trim().isEmpty()) {
            return null;
        }
        String likePattern = "%" + searchQuery.trim().toLowerCase() + "%";
        return (root, query, cb) -> cb.or(
            cb.like(cb.lower(root.get("name")), likePattern),
            cb.like(cb.lower(root.get("venue")), likePattern)
        );
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
}

