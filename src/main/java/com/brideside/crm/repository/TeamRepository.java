package com.brideside.crm.repository;

import com.brideside.crm.entity.Team;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface TeamRepository extends JpaRepository<Team, Long> {
    List<Team> findByManager_Id(Long managerId);
    List<Team> findByMembers_Id(Long memberId);
    boolean existsByMembers_Id(Long memberId);
    
    @Query("SELECT t FROM Team t INNER JOIN FETCH t.manager WHERE t.id = :id")
    Optional<Team> findByIdWithManager(@Param("id") Long id);
    
    @Query("SELECT DISTINCT t FROM Team t LEFT JOIN FETCH t.manager LEFT JOIN FETCH t.members WHERE t.id = :id")
    Optional<Team> findByIdWithMembers(@Param("id") Long id);
}

