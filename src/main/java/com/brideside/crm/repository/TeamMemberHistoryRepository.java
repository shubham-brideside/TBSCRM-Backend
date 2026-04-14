package com.brideside.crm.repository;

import com.brideside.crm.entity.TeamMemberHistory;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;

public interface TeamMemberHistoryRepository extends JpaRepository<TeamMemberHistory, Long> {

    List<TeamMemberHistory> findByTeam_IdAndEffectiveToIsNull(Long teamId);

    @Query("select h from TeamMemberHistory h " +
            "where h.manager.id in :managerIds " +
            "and h.effectiveFrom <= :rangeEnd " +
            "and (h.effectiveTo is null or h.effectiveTo >= :rangeStart)")
    List<TeamMemberHistory> findOverlappingByManagerIds(
            @Param("managerIds") List<Long> managerIds,
            @Param("rangeStart") LocalDateTime rangeStart,
            @Param("rangeEnd") LocalDateTime rangeEnd
    );
}
