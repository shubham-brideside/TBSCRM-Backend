package com.brideside.crm.repository;

import com.brideside.crm.entity.Person;
import org.springframework.data.jpa.domain.Specification;

import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
import java.time.Instant;
import java.time.LocalDate;
import java.util.List;

public final class PersonSpecifications {

    private PersonSpecifications() {
    }

    /**
     * Excludes soft-deleted persons from queries
     */
    public static Specification<Person> notDeleted() {
        return (root, query, cb) -> cb.or(
            cb.isNull(root.get("isDeleted")),
            cb.equal(root.get("isDeleted"), false)
        );
    }

    public static Specification<Person> search(String q) {
        if (q == null || q.isBlank()) {
            return null;
        }
        String like = "%" + q.toLowerCase() + "%";
        return (root, query, cb) -> {
            Join<Object, Object> organizationJoin = root.join("organization", JoinType.LEFT);
            Join<Object, Object> ownerJoin = root.join("owner", JoinType.LEFT);
            return cb.or(
                    cb.like(cb.lower(root.get("name")), like),
                    cb.like(cb.lower(root.get("instagramId")), like),
                    cb.like(cb.lower(root.get("phone")), like),
                    cb.like(cb.lower(root.get("email")), like),
                    cb.like(cb.lower(organizationJoin.get("name")), like),
                    cb.like(cb.lower(ownerJoin.get("firstName")), like),
                    cb.like(cb.lower(ownerJoin.get("lastName")), like)
            );
        };
    }

    public static Specification<Person> hasLabel(Person.PersonLabel label) {
        if (label == null) {
            return null;
        }
        return (root, query, cb) -> cb.equal(root.get("label"), label);
    }

    public static Specification<Person> hasLabels(List<Person.PersonLabel> labels) {
        if (labels == null || labels.isEmpty()) {
            return null;
        }
        return (root, query, cb) -> root.get("label").in(labels);
    }

    public static Specification<Person> hasSource(com.brideside.crm.entity.DealSource source) {
        if (source == null) {
            return null;
        }
        return (root, query, cb) -> cb.equal(root.get("source"), source);
    }

    public static Specification<Person> hasOrganization(Long organizationId) {
        if (organizationId == null) {
            return null;
        }
        return (root, query, cb) -> cb.equal(root.join("organization", JoinType.LEFT).get("id"), organizationId);
    }

    public static Specification<Person> hasOrganizations(List<Long> organizationIds) {
        if (organizationIds == null || organizationIds.isEmpty()) {
            return null;
        }
        return (root, query, cb) -> root.join("organization", JoinType.LEFT).get("id").in(organizationIds);
    }

    public static Specification<Person> hasOwner(Long ownerId) {
        if (ownerId == null) {
            return null;
        }
        return (root, query, cb) -> cb.equal(root.join("owner", JoinType.LEFT).get("id"), ownerId);
    }

    public static Specification<Person> hasOwners(List<Long> ownerIds) {
        if (ownerIds == null || ownerIds.isEmpty()) {
            return null;
        }
        return (root, query, cb) -> root.join("owner", JoinType.LEFT).get("id").in(ownerIds);
    }

    public static Specification<Person> hasCategory(Long categoryId) {
        if (categoryId == null) {
            return null;
        }
        return (root, query, cb) -> cb.equal(root.join("category", JoinType.LEFT).get("id"), categoryId);
    }

    public static Specification<Person> hasCategories(List<Long> categoryIds) {
        if (categoryIds == null || categoryIds.isEmpty()) {
            return null;
        }
        return (root, query, cb) -> root.join("category", JoinType.LEFT).get("id").in(categoryIds);
    }

    public static Specification<Person> leadDateBetween(LocalDate fromInclusive, LocalDate toInclusive) {
        if (fromInclusive == null && toInclusive == null) {
            return null;
        }
        return (root, query, cb) -> {
            if (fromInclusive != null && toInclusive != null) {
                return cb.between(root.get("leadDate"), fromInclusive, toInclusive);
            } else if (fromInclusive != null) {
                return cb.greaterThanOrEqualTo(root.get("leadDate"), fromInclusive);
            } else {
                return cb.lessThanOrEqualTo(root.get("leadDate"), toInclusive);
            }
        };
    }

    public static Specification<Person> createdAtBetween(Instant fromInclusive, Instant toExclusive) {
        if (fromInclusive == null && toExclusive == null) {
            return null;
        }
        return (root, query, cb) -> {
            if (fromInclusive != null && toExclusive != null) {
                return cb.and(
                        cb.greaterThanOrEqualTo(root.get("createdAt"), fromInclusive),
                        cb.lessThan(root.get("createdAt"), toExclusive)
                );
            } else if (fromInclusive != null) {
                return cb.greaterThanOrEqualTo(root.get("createdAt"), fromInclusive);
            } else {
                return cb.lessThan(root.get("createdAt"), toExclusive);
            }
        };
    }

    /**
     * Filter persons who have at least one deal with the specified source
     */
    public static Specification<Person> hasDealSource(com.brideside.crm.entity.DealSource dealSource) {
        if (dealSource == null) {
            return null;
        }
        return (root, query, cb) -> {
            // Use subquery to find persons who have deals with the specified source
            jakarta.persistence.criteria.Subquery<Long> dealSubquery = query.subquery(Long.class);
            jakarta.persistence.criteria.Root<com.brideside.crm.entity.Deal> dealRoot = dealSubquery.from(com.brideside.crm.entity.Deal.class);
            dealSubquery.select(cb.literal(1L))
                    .where(cb.and(
                            cb.equal(dealRoot.get("dealSource"), dealSource),
                            cb.or(
                                    cb.isNull(dealRoot.get("isDeleted")),
                                    cb.equal(dealRoot.get("isDeleted"), false)
                            ),
                            cb.equal(dealRoot.get("person").get("id"), root.get("id"))
                    ));
            return cb.exists(dealSubquery);
        };
    }
}

