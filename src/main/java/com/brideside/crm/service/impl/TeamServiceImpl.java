package com.brideside.crm.service.impl;

import com.brideside.crm.dto.TeamDtos;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.Team;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.PipelineRepository;
import com.brideside.crm.repository.TeamRepository;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.TeamService;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Transactional
public class TeamServiceImpl implements TeamService {

    private final TeamRepository teamRepository;
    private final UserRepository userRepository;
    private final PipelineRepository pipelineRepository;

    public TeamServiceImpl(TeamRepository teamRepository,
                           UserRepository userRepository,
                           PipelineRepository pipelineRepository) {
        this.teamRepository = teamRepository;
        this.userRepository = userRepository;
        this.pipelineRepository = pipelineRepository;
    }

    @Override
    public TeamDtos.TeamResponse create(TeamDtos.TeamRequest request) {
        Team team = new Team();
        team.setName(request.getName().trim());
        team.setManager(resolveManager(request.getManagerId()));
        team.setMembers(resolveMembers(request.getMemberIds()));
        ensureManagerAsMember(team);
        return TeamDtos.toResponse(teamRepository.save(team));
    }

    @Override
    @Transactional(readOnly = true)
    public List<TeamDtos.TeamResponse> list() {
        return TeamDtos.toResponses(teamRepository.findAll(Sort.by(Sort.Direction.ASC, "name")));
    }

    @Override
    @Transactional(readOnly = true)
    public TeamDtos.TeamResponse get(Long id) {
        return TeamDtos.toResponse(getOrThrow(id));
    }

    @Override
    public TeamDtos.TeamResponse update(Long id, TeamDtos.TeamRequest request) {
        Team team = getOrThrow(id);
        if (request.getName() != null) {
            team.setName(request.getName().trim());
        }
        if (request.getManagerId() != null) {
            team.setManager(resolveManager(request.getManagerId()));
        } else if (Boolean.TRUE.equals(request.getClearManager())) {
            team.setManager(null);
        }
        if (request.getMemberIds() != null) {
            team.setMembers(resolveMembers(request.getMemberIds()));
        }
        ensureManagerAsMember(team);
        return TeamDtos.toResponse(teamRepository.save(team));
    }

    @Override
    public void delete(Long id) {
        Team team = getOrThrow(id);
        if (pipelineRepository.existsByTeam(team)) {
            throw new BadRequestException("Cannot delete team assigned to pipelines");
        }
        teamRepository.delete(team);
    }

    @Override
    @Transactional(readOnly = true)
    public List<TeamDtos.UserSummary> listManagersForRole(Role.RoleName roleName) {
        if (roleName == null) {
            roleName = Role.RoleName.SALES;
        }
        List<User> users;
        switch (roleName) {
            case ADMIN:
            case CATEGORY_MANAGER:
                return List.of();
            case SALES:
            case PRESALES:
                users = userRepository.findByRole_NameAndActiveTrue(Role.RoleName.SALES);
                break;
            default:
                return List.of();
        }
        return users.stream()
                .map(TeamDtos::toSummary)
                .collect(Collectors.toList());
    }

    @Override
    @Transactional(readOnly = true)
    public List<TeamDtos.UserSummary> listMembers() {
        return userRepository.findByRole_NameAndActiveTrue(Role.RoleName.PRESALES)
                .stream()
                .filter(user -> user.getId() != null && !teamRepository.existsByMembers_Id(user.getId()))
                .map(TeamDtos::toSummary)
                .collect(Collectors.toList());
    }

    private Team getOrThrow(Long id) {
        return teamRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Team not found with id " + id));
    }

    private User resolveManager(Long managerId) {
        if (managerId == null) return null;
        User manager = userRepository.findById(managerId)
                .orElseThrow(() -> new BadRequestException("Manager not found with id " + managerId));
        if (Boolean.FALSE.equals(manager.getActive())) {
            throw new BadRequestException("Manager must be an active user");
        }
        if (manager.getRole() == null) {
            throw new BadRequestException("Manager must have a role");
        }
        Role.RoleName role = manager.getRole().getName();
        if (role != Role.RoleName.SALES) {
            throw new BadRequestException("Manager must have SALES role");
        }
        return manager;
    }

    private Set<User> resolveMembers(List<Long> memberIds) {
        if (memberIds == null || memberIds.isEmpty()) return new HashSet<>();
        Set<Long> uniqueIds = new HashSet<>(memberIds);
        List<User> users = userRepository.findAllById(uniqueIds);
        if (users.size() != uniqueIds.size()) {
            throw new BadRequestException("One or more member ids are invalid");
        }
        users.forEach(user -> {
            if (Boolean.FALSE.equals(user.getActive())) {
                throw new BadRequestException("Team members must be active users");
            }
            if (user.getRole() == null || user.getRole().getName() != Role.RoleName.PRESALES) {
                throw new BadRequestException("Team members must have PRESALES role");
            }
        });
        return new HashSet<>(users);
    }

    private void ensureManagerAsMember(Team team) {
        if (team.getManager() == null) return;
        if (team.getMembers() == null) {
            team.setMembers(new HashSet<>());
        }
        team.getMembers().add(team.getManager());
    }
}


