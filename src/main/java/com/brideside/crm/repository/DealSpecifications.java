package com.brideside.crm.repository;

import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.DealStatus;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Pipeline;
import com.brideside.crm.service.DealAccessScope;
import org.springframework.data.jpa.domain.Specification;

import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.criteria.Subquery;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

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
     * Filter by pipeline ID. Matches deals with {@code pipeline_id = pipelineId}, and also deals with no
     * pipeline set whose {@code organization_id} matches that pipeline's organization (common legacy data).
     */
    public static Specification<Deal> hasPipeline(Long pipelineId) {
        if (pipelineId == null) {
            return null;
        }
        return (root, query, cb) -> {
            Subquery<Long> pipelineOrgSub = query.subquery(Long.class);
            Root<Pipeline> pipeRoot = pipelineOrgSub.from(Pipeline.class);
            pipelineOrgSub.select(pipeRoot.get("organization").get("id"));
            pipelineOrgSub.where(cb.equal(pipeRoot.get("id"), pipelineId));

            return cb.or(
                    cb.equal(root.get("pipelineId"), pipelineId),
                    cb.and(
                            cb.isNull(root.get("pipelineId")),
                            cb.isNotNull(root.get("organizationId")),
                            cb.equal(root.get("organizationId"), pipelineOrgSub)
                    )
            );
        };
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
     * Filter by manager ID (deal's denormalized owner_id).
     */
    public static Specification<Deal> hasManager(Long managerId) {
        if (managerId == null) {
            return null;
        }
        return (root, query, cb) -> cb.equal(root.get("ownerId"), managerId);
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
     * Search across multiple fields: deal name, venue, phone number, person name, person instagram ID, and organization name (case-insensitive)
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
            
            // Search in deal phone number
            jakarta.persistence.criteria.Predicate phoneNumberPredicate = cb.like(
                cb.lower(root.get("phoneNumber")), likePattern
            );
            
            // Search in person name, instagram ID, and phone (if person exists)
            Join<Object, Object> personJoin = root.join("person", JoinType.LEFT);
            jakarta.persistence.criteria.Predicate personNamePredicate = cb.like(
                cb.lower(root.get("personName")), likePattern
            );
            jakarta.persistence.criteria.Predicate personInstagramPredicate = cb.like(
                cb.lower(personJoin.get("instagramId")), likePattern
            );
            jakarta.persistence.criteria.Predicate personPhonePredicate = cb.like(
                cb.lower(personJoin.get("phone")), likePattern
            );
            
            // Search in organization name (if organization exists)
            Join<Object, Object> orgJoin = root.join("organization", JoinType.LEFT);
            jakarta.persistence.criteria.Predicate orgNamePredicate = cb.like(
                cb.lower(orgJoin.get("name")), likePattern
            );
            
            return cb.or(dealNamePredicate, venuePredicate, phoneNumberPredicate, 
                         personNamePredicate, personInstagramPredicate, personPhonePredicate, 
                         orgNamePredicate);
        };
    }

    /**
     * Focused search for global search API - only searches in deal name, phone number, person name, person instagram ID, and person phone
     * Does not search in venue or organization name to avoid unrelated results
     */
    public static Specification<Deal> focusedSearch(String searchQuery) {
        if (searchQuery == null || searchQuery.trim().isEmpty()) {
            return null;
        }
        String likePattern = "%" + searchQuery.trim().toLowerCase() + "%";
        return (root, query, cb) -> {
            // Search in deal name
            jakarta.persistence.criteria.Predicate dealNamePredicate = cb.like(cb.lower(root.get("name")), likePattern);
            
            // Search in deal phone number
            jakarta.persistence.criteria.Predicate phoneNumberPredicate = cb.like(
                cb.lower(root.get("phoneNumber")), likePattern
            );
            
            // Search in person name, instagram ID, and phone (if person exists)
            Join<Object, Object> personJoin = root.join("person", JoinType.LEFT);
            jakarta.persistence.criteria.Predicate personNamePredicate = cb.like(
                cb.lower(root.get("personName")), likePattern
            );
            jakarta.persistence.criteria.Predicate personInstagramPredicate = cb.like(
                cb.lower(personJoin.get("instagramId")), likePattern
            );
            jakarta.persistence.criteria.Predicate personPhonePredicate = cb.like(
                cb.lower(personJoin.get("phone")), likePattern
            );
            
            return cb.or(dealNamePredicate, phoneNumberPredicate, 
                         personNamePredicate, personInstagramPredicate, personPhonePredicate);
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

    /**
     * Filter deals by organization owner ID or person owner ID.
     * Returns deals where:
     * - The deal's organization is owned by any of the specified owner IDs, OR
     * - The deal's person is owned by any of the specified owner IDs
     * Returns null if ownerIds is null (no restrictions) or empty (no access).
     * Handles null organizations and persons gracefully using LEFT JOINs.
     */
    public static Specification<Deal> hasOrganizationOrPersonOwnerIn(List<Long> ownerIds) {
        if (ownerIds == null) {
            // null means no restrictions (Admin)
            return null;
        }
        if (ownerIds.isEmpty()) {
            // Empty list means no access
            return (root, query, cb) -> cb.disjunction(); // Always false
        }
        return (root, query, cb) -> {
            // Condition 1: Deal's organization owner is in the list (only if organization exists)
            Join<Object, Object> organizationJoin = root.join("organization", JoinType.LEFT);
            jakarta.persistence.criteria.Predicate orgOwnerMatch = cb.and(
                cb.isNotNull(organizationJoin.get("owner")),
                organizationJoin.get("owner").get("id").in(ownerIds)
            );
            
            // Condition 2: Deal's person owner is in the list (only if person exists)
            Join<Object, Object> personJoin = root.join("person", JoinType.LEFT);
            jakarta.persistence.criteria.Predicate personOwnerMatch = cb.and(
                cb.isNotNull(personJoin.get("owner")),
                personJoin.get("owner").get("id").in(ownerIds)
            );

            // Condition 3: Deal's denormalized owner_id (kept in sync with person/org)
            jakarta.persistence.criteria.Predicate dealOwnerMatch = cb.and(
                cb.isNotNull(root.get("ownerId")),
                root.get("ownerId").in(ownerIds)
            );
            
            // Return OR of all conditions
            return cb.or(orgOwnerMatch, personOwnerMatch, dealOwnerMatch);
        };
    }

    /**
     * Limits deals to pipelines (and optional org fallback for deals with no pipeline) per {@link DealAccessScope}.
     */
    public static Specification<Deal> restrictedToDealAccessScope(DealAccessScope scope) {
        if (scope == null || scope.fullAccess()) {
            return null;
        }
        // Vertical category managers: category-only spec (do not OR with org-wide pipelineOrg spec).
        if (scope.categoryManagerCategoryScope() != null) {
            return categoryManagerCategoryScopeSpec(scope);
        }
        return pipelineOrgOnlyDealAccessSpec(scope);
    }

    /** Pipeline id set + optional org-based access (sales/presales and org-tree category managers). */
    private static Specification<Deal> pipelineOrgOnlyDealAccessSpec(DealAccessScope scope) {
        var pids = scope.allowedPipelineIds();
        var orgIds = scope.nullablePipelineOrganizationIds();
        boolean hasPipelines = pids != null && !pids.isEmpty();
        boolean hasNullPipelineOrgs = orgIds != null && !orgIds.isEmpty();
        if (!hasPipelines && !hasNullPipelineOrgs) {
            return (root, query, cb) -> cb.disjunction();
        }
        return (root, query, cb) -> {
            List<Predicate> parts = new ArrayList<>();
            if (hasPipelines) {
                parts.add(root.get("pipelineId").in(pids));
            }
            if (scope.matchDealsByOrganizationId() && hasNullPipelineOrgs) {
                parts.add(root.get("organizationId").in(orgIds));
            } else if (hasNullPipelineOrgs) {
                parts.add(cb.and(
                        cb.isNull(root.get("pipelineId")),
                        root.get("organizationId").in(orgIds)));
            }
            if (parts.isEmpty()) {
                return cb.disjunction();
            }
            return cb.or(parts.toArray(Predicate[]::new));
        };
    }

    /**
     * Category managers with {@code users.user_managed_category_id}: pipelines with matching
     * {@link Pipeline#getCategory()}, deals with matching {@link com.brideside.crm.entity.Category},
     * or organizations with matching {@link Organization.OrganizationCategory}.
     */
    private static Specification<Deal> categoryManagerCategoryScopeSpec(DealAccessScope scope) {
        DealAccessScope.CategoryManagerCategoryScope cm = scope.categoryManagerCategoryScope();
        if (cm == null) {
            return (root, query, cb) -> cb.disjunction();
        }
        Long categoryId = cm.categoryId();
        String categoryName = cm.categoryName() != null ? cm.categoryName().trim() : "";
        Organization.OrganizationCategory orgEnum = categoryName.isEmpty()
                ? null
                : Organization.OrganizationCategory.fromDbValue(categoryName);

        return (root, query, cb) -> {
            if (query != null) {
                query.distinct(true);
            }
            List<Predicate> parts = new ArrayList<>();
            Set<Long> pids = scope.allowedPipelineIds();
            if (pids != null && !pids.isEmpty()) {
                parts.add(root.get("pipelineId").in(pids));
            }
            if (categoryId != null) {
                parts.add(cb.equal(root.get("categoryId"), categoryId));
            }
            if (!categoryName.isEmpty()) {
                var pl = root.join("pipeline", JoinType.LEFT);
                parts.add(cb.and(
                        cb.isNotNull(root.get("pipelineId")),
                        cb.isNotNull(pl.get("category")),
                        cb.equal(cb.lower(pl.get("category")), categoryName.toLowerCase())));
            }
            if (orgEnum != null) {
                var org = root.join("organization", JoinType.LEFT);
                parts.add(cb.equal(org.get("category"), orgEnum));
            }
            if (parts.isEmpty()) {
                return cb.disjunction();
            }
            return cb.or(parts.toArray(Predicate[]::new));
        };
    }
}

