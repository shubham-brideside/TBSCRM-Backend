package com.brideside.crm.repository;

import com.brideside.crm.entity.Pipeline;
import com.brideside.crm.entity.Team;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface PipelineRepository extends JpaRepository<Pipeline, Long> {
    boolean existsByNameIgnoreCase(String name);
    boolean existsByNameIgnoreCaseAndIdNot(String name, Long id);
    List<Pipeline> findByDeletedFalseOrderByNameAsc();
    boolean existsByTeam(Team team);
}
