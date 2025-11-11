package com.brideside.crm.repository;

import com.brideside.crm.entity.Person;
import org.springframework.data.jpa.domain.Specification;

import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
import java.time.Instant;
import java.time.LocalDate;

public final class PersonSpecifications {

    private PersonSpecifications() {
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

    public static Specification<Person> hasSource(Person.PersonSource source) {
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

    public static Specification<Person> hasOwner(Long ownerId) {
        if (ownerId == null) {
            return null;
        }
        return (root, query, cb) -> cb.equal(root.join("owner", JoinType.LEFT).get("id"), ownerId);
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
}

