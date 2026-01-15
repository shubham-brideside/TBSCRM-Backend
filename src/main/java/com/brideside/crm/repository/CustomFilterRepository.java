package com.brideside.crm.repository;

import com.brideside.crm.entity.CustomFilter;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface CustomFilterRepository extends JpaRepository<CustomFilter, Long> {
    List<CustomFilter> findByUserId(Long userId);
    List<CustomFilter> findByUserIdAndEntityType(Long userId, String entityType);
    Optional<CustomFilter> findByUserIdAndFilterNameAndEntityType(Long userId, String filterName, String entityType);
    boolean existsByUserIdAndFilterNameAndEntityType(Long userId, String filterName, String entityType);
    void deleteByUserIdAndFilterNameAndEntityType(Long userId, String filterName, String entityType);
}
