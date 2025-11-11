package com.brideside.crm.service;

import com.brideside.crm.dto.TeamDtos;
import com.brideside.crm.entity.Role;

import java.util.List;

public interface TeamService {
    TeamDtos.TeamResponse create(TeamDtos.TeamRequest request);
    List<TeamDtos.TeamResponse> list();
    TeamDtos.TeamResponse get(Long id);
    TeamDtos.TeamResponse update(Long id, TeamDtos.TeamRequest request);
    void delete(Long id);
    List<TeamDtos.UserSummary> listManagersForRole(Role.RoleName roleName);
    List<TeamDtos.UserSummary> listMembers();
}


