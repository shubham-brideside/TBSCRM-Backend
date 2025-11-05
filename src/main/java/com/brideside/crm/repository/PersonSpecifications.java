package com.brideside.crm.repository;

import com.brideside.crm.entity.Person;
import org.springframework.data.jpa.domain.Specification;

public class PersonSpecifications {
    public static Specification<Person> search(String q) {
        if (q == null || q.isBlank()) return null;
        String like = "%" + q.toLowerCase() + "%";
        return (root, query, cb) -> cb.or(
            cb.like(cb.lower(root.get("name")), like),
            cb.like(cb.lower(root.get("instagramId")), like),
            cb.like(cb.lower(root.get("phone")), like),
            cb.like(cb.lower(root.get("organization")), like),
            cb.like(cb.lower(root.get("manager")), like),
            cb.like(cb.lower(root.get("category")), like),
            cb.like(cb.lower(root.get("source")), like),
            cb.like(cb.lower(root.get("eventType")), like)
        );
    }

    public static Specification<Person> equalsField(String field, String value) {
        if (value == null || value.isBlank()) return null;
        return (root, query, cb) -> cb.equal(root.get(field), value);
    }

    public static Specification<Person> createdAtBetween(java.time.Instant fromInclusive, java.time.Instant toExclusive) {
        if (fromInclusive == null && toExclusive == null) return null;
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


