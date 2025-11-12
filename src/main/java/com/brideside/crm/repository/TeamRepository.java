package com.brideside.crm.repository;

import com.brideside.crm.entity.Team;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface TeamRepository extends JpaRepository<Team, Long> {
    List<Team> findByManager_Id(Long managerId);
    List<Team> findByMembers_Id(Long memberId);
}

