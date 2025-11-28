package com.brideside.crm.repository;

import com.brideside.crm.entity.SalesTarget;
import com.brideside.crm.entity.TargetCategory;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface SalesTargetRepository extends JpaRepository<SalesTarget, Long> {
    // Legacy methods for backward compatibility
    @Deprecated
    @Query("SELECT st FROM SalesTarget st WHERE st.user.id = :userId AND st.category = :category AND st.periodStart = :monthStart")
    Optional<SalesTarget> findByUser_IdAndCategoryAndMonthStart(@Param("userId") Long userId, @Param("category") TargetCategory category, @Param("monthStart") LocalDate monthStart);
    
    @Deprecated
    @Query("SELECT st FROM SalesTarget st WHERE st.periodStart = :monthStart")
    List<SalesTarget> findByMonthStart(@Param("monthStart") LocalDate monthStart);
    
    @Deprecated
    @Query("SELECT st FROM SalesTarget st WHERE st.periodStart BETWEEN :startInclusive AND :endInclusive")
    List<SalesTarget> findByMonthStartBetween(@Param("startInclusive") LocalDate startInclusive, @Param("endInclusive") LocalDate endInclusive);
    
    @Deprecated
    @Query("SELECT st FROM SalesTarget st WHERE st.category = :category AND st.periodStart BETWEEN :startInclusive AND :endInclusive")
    List<SalesTarget> findByCategoryAndMonthStartBetween(@Param("category") TargetCategory category, @Param("startInclusive") LocalDate startInclusive, @Param("endInclusive") LocalDate endInclusive);
    
    // New methods with period type
    Optional<SalesTarget> findByUser_IdAndCategoryAndPeriodTypeAndPeriodStart(
            Long userId, TargetCategory category, SalesTarget.PeriodType periodType, LocalDate periodStart);
    
    List<SalesTarget> findByPeriodStart(LocalDate periodStart);
    
    List<SalesTarget> findByPeriodStartBetween(LocalDate startInclusive, LocalDate endInclusive);
    
    List<SalesTarget> findByCategoryAndPeriodStartBetween(TargetCategory category, LocalDate startInclusive, LocalDate endInclusive);
    
    List<SalesTarget> findByUser_Id(Long userId);
    
    List<SalesTarget> findByUser_IdAndPeriodStartBetween(Long userId, LocalDate startInclusive, LocalDate endInclusive);
    
    @Query("SELECT DISTINCT st FROM SalesTarget st JOIN st.organizations org WHERE org.id IN :organizationIds")
    List<SalesTarget> findByOrganizationIds(@Param("organizationIds") List<Long> organizationIds);
}

