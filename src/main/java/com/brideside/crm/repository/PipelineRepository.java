package com.brideside.crm.repository;

import com.brideside.crm.entity.Pipeline;
import com.brideside.crm.entity.Team;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface PipelineRepository extends JpaRepository<Pipeline, Long> {
    boolean existsByNameIgnoreCase(String name);
    boolean existsByNameIgnoreCaseAndIdNot(String name, Long id);
    List<Pipeline> findByDeletedFalseOrderByNameAsc();
    List<Pipeline> findByDeletedTrueOrderByNameAsc();
    boolean existsByTeam(Team team);
    
    @Query("SELECT p FROM Pipeline p INNER JOIN FETCH p.team WHERE p.id = :id")
    Optional<Pipeline> findByIdWithTeam(@Param("id") Long id);
}
