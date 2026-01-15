package com.brideside.crm.service.impl;

import com.brideside.crm.dto.TeamDtos;
import com.brideside.crm.entity.Pipeline;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.Team;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.PipelineRepository;
import com.brideside.crm.repository.TeamRepository;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.TeamService;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
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
    
    @PersistenceContext
    private EntityManager entityManager;

    public TeamServiceImpl(TeamRepository teamRepository,
                           UserRepository userRepository,
                           PipelineRepository pipelineRepository) {
        this.teamRepository = teamRepository;
        this.userRepository = userRepository;
        this.pipelineRepository = pipelineRepository;
    }

    @Override
    public TeamDtos.TeamResponse create(TeamDtos.TeamRequest request) {
        // Require at least one of: manager or member
        boolean hasManager = request.getManagerId() != null;
        boolean hasMembers = request.getMemberIds() != null && !request.getMemberIds().isEmpty();
        if (!hasManager && !hasMembers) {
            throw new BadRequestException("Team must have at least a manager or one member");
        }

        Team team = new Team();
        team.setName(request.getName().trim());
        team.setManager(resolveManager(request.getManagerId()));
        team.setMembers(resolveMembers(request.getMemberIds(), request.getManagerId()));
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
        // Fetch team with members loaded to ensure proper collection management
        Team team = request.getMemberIds() != null 
                ? getOrThrowWithMembers(id) 
                : getOrThrow(id);
        // Track effective manager id during this update so we can exclude it from member validation
        Long effectiveManagerId = team.getManager() != null ? team.getManager().getId() : null;

        if (request.getName() != null) {
            team.setName(request.getName().trim());
        }
        if (request.getManagerId() != null) {
            team.setManager(resolveManager(request.getManagerId()));
            effectiveManagerId = request.getManagerId();
        } else if (Boolean.TRUE.equals(request.getClearManager())) {
            team.setManager(null);
            effectiveManagerId = null;
        }
        if (request.getMemberIds() != null) {
            // Replace members with exactly the provided memberIds
            // This handles:
            // - Empty arrays []: removes all members (manager will be re-added by ensureManagerAsMember)
            // - Arrays with fewer IDs: removes members not in the list
            // - Arrays with different IDs: replaces all members with the new list
            // Get the new members to add (excluding manager, which is handled separately)
            Set<User> newMembers = resolveMembers(request.getMemberIds(), effectiveManagerId);
            
            // Force initialization of the collection by accessing it
            // This ensures we're working with the actual Hibernate-managed collection
            team.getMembers().size();
            
            // Create a completely new HashSet to replace the collection
            // This forces Hibernate to recognize it as a complete replacement
            // and properly delete old join table entries and insert new ones
            Set<User> replacementMembers = new HashSet<>(newMembers);
            team.setMembers(replacementMembers);
        }
        // Business rule: manager must always be a member (added if not already present)
        ensureManagerAsMember(team);
        return TeamDtos.toResponse(teamRepository.save(team));
    }

    @Override
    public void delete(Long id) {
        Team team = getOrThrow(id);
        // Detach this team from any pipelines that currently reference it
        List<Pipeline> pipelines = pipelineRepository.findByTeam(team);
        if (!pipelines.isEmpty()) {
            for (Pipeline pipeline : pipelines) {
                pipeline.setTeam(null);
            }
            pipelineRepository.saveAll(pipelines);
        }
        teamRepository.delete(team);
    }

    @Override
    @Transactional(readOnly = true)
    public List<TeamDtos.UserSummary> listManagersForRole(Role.RoleName roleName) {
        if (roleName == null) {
            // If no role specified, return all eligible managers (ADMIN, CATEGORY_MANAGER, SALES)
            List<User> allManagers = new java.util.ArrayList<>();
            allManagers.addAll(userRepository.findByRole_NameAndActiveTrue(Role.RoleName.ADMIN));
            allManagers.addAll(userRepository.findByRole_NameAndActiveTrue(Role.RoleName.CATEGORY_MANAGER));
            allManagers.addAll(userRepository.findByRole_NameAndActiveTrue(Role.RoleName.SALES));
            return allManagers.stream()
                    .map(TeamDtos::toSummary)
                    .collect(Collectors.toList());
        }
        List<User> users;
        switch (roleName) {
            case ADMIN:
                users = userRepository.findByRole_NameAndActiveTrue(Role.RoleName.ADMIN);
                break;
            case CATEGORY_MANAGER:
                users = userRepository.findByRole_NameAndActiveTrue(Role.RoleName.CATEGORY_MANAGER);
                break;
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

    private Team getOrThrowWithMembers(Long id) {
        return teamRepository.findByIdWithMembers(id)
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
        // Allow ADMIN, CATEGORY_MANAGER, or SALES roles for team managers
        if (role != Role.RoleName.ADMIN && role != Role.RoleName.CATEGORY_MANAGER && role != Role.RoleName.SALES) {
            throw new BadRequestException("Manager must have ADMIN, CATEGORY_MANAGER, or SALES role");
        }
        return manager;
    }

    /**
     * Resolve team members from ids.
     * <p>
     * Business rules:
     * - Team members can have any role.
     * - The team manager (ADMIN, CATEGORY_MANAGER, or SALES) is always added separately via {@link #ensureManagerAsMember(Team)}
     *   and should be ignored during member validation to avoid false validation errors when the
     *   frontend sends the full member list including the manager.
     *
     * @param memberIds           ids of members coming from the request (may include the manager id)
     * @param managerIdToExclude  current/effective manager id; if present it will be ignored from validation
     */
    private Set<User> resolveMembers(List<Long> memberIds, Long managerIdToExclude) {
        if (memberIds == null || memberIds.isEmpty()) return new HashSet<>();
        Set<Long> uniqueIds = new HashSet<>(memberIds);

        // The manager is handled separately.
        // If the incoming list includes the manager id (common during updates),
        // remove it before validating members to avoid spurious errors.
        if (managerIdToExclude != null) {
            uniqueIds.remove(managerIdToExclude);
        }

        if (uniqueIds.isEmpty()) {
            return new HashSet<>();
        }

        List<User> users = userRepository.findAllById(uniqueIds);
        if (users.size() != uniqueIds.size()) {
            throw new BadRequestException("One or more member ids are invalid");
        }
        users.forEach(user -> {
            if (Boolean.FALSE.equals(user.getActive())) {
                throw new BadRequestException("Team members must be active users");
            }
            if (user.getRole() == null) {
                throw new BadRequestException("Team members must have a role");
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


