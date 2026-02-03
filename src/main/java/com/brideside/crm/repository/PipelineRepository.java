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

    // Pipelines assigned to a given team
    List<Pipeline> findByTeam(Team team);
    
    @Query("SELECT p FROM Pipeline p INNER JOIN FETCH p.team WHERE p.id = :id")
    Optional<Pipeline> findByIdWithTeam(@Param("id") Long id);

    // Pipelines belonging to a given organization
    List<Pipeline> findByOrganization(com.brideside.crm.entity.Organization organization);
    
    // Pipelines assigned to teams with given IDs
    @Query("SELECT p FROM Pipeline p WHERE p.deleted = false AND p.team IS NOT NULL AND p.team.id IN :teamIds ORDER BY p.name ASC")
    List<Pipeline> findByDeletedFalseAndTeam_IdInOrderByNameAsc(@Param("teamIds") List<Long> teamIds);
    
    // Pipelines with a specific category
    @Query("SELECT p FROM Pipeline p WHERE p.deleted = false AND p.category = :category ORDER BY p.name ASC")
    List<Pipeline> findByDeletedFalseAndCategoryOrderByNameAsc(@Param("category") String category);
}
