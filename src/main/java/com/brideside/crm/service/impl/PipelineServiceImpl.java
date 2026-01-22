package com.brideside.crm.service.impl;

import com.brideside.crm.dto.PipelineDtos;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Pipeline;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.Stage;
import com.brideside.crm.entity.Team;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.mapper.PipelineMapper;
import com.brideside.crm.repository.DealRepository;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.repository.PipelineRepository;
import com.brideside.crm.repository.StageRepository;
import com.brideside.crm.repository.TeamRepository;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.PipelineService;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Transactional
public class PipelineServiceImpl implements PipelineService {

    private final PipelineRepository pipelineRepository;
    private final StageRepository stageRepository;
    private final OrganizationRepository organizationRepository;
    private final TeamRepository teamRepository;
    private final DealRepository dealRepository;
    private final UserRepository userRepository;

    public PipelineServiceImpl(PipelineRepository pipelineRepository,
                               StageRepository stageRepository,
                               OrganizationRepository organizationRepository,
                               TeamRepository teamRepository,
                               DealRepository dealRepository,
                               UserRepository userRepository) {
        this.pipelineRepository = pipelineRepository;
        this.stageRepository = stageRepository;
        this.organizationRepository = organizationRepository;
        this.teamRepository = teamRepository;
        this.dealRepository = dealRepository;
        this.userRepository = userRepository;
    }

    @Override
    public PipelineDtos.PipelineResponse createPipeline(PipelineDtos.PipelineRequest request) {
        validatePipelineName(request.getName(), null);

        Pipeline pipeline = new Pipeline();
        pipeline.setName(request.getName().trim());
        pipeline.setCategory(trimToNull(request.getCategory()));
        pipeline.setTeam(resolveTeam(request.getTeamId()));
        pipeline.setOrganization(resolveOrganization(request.getOrganizationId()));
        pipeline.setDeleted(Boolean.FALSE);

        Pipeline saved = pipelineRepository.save(pipeline);
        List<Stage> stages = createDefaultStages(saved);
        return PipelineMapper.toPipelineResponse(saved, stages, true);
    }

    @Override
    @Transactional(readOnly = true)
    public List<PipelineDtos.PipelineResponse> listPipelines(boolean includeStages) {
        List<Pipeline> pipelines = getFilteredPipelines();

        Map<Long, List<Stage>> stagesByPipeline = includeStages
                ? loadStagesForPipelines(pipelines, false)
                : Collections.emptyMap();

        return pipelines.stream()
                .map(p -> PipelineMapper.toPipelineResponse(
                        p,
                        stagesByPipeline.getOrDefault(p.getId(), Collections.emptyList()),
                        includeStages
                ))
                .collect(Collectors.toList());
    }

    @Override
    @Transactional(readOnly = true)
    public List<PipelineDtos.PipelineResponse> listArchivedPipelines(boolean includeStages) {
        List<Pipeline> pipelines = pipelineRepository.findByDeletedTrueOrderByNameAsc();

        Map<Long, List<Stage>> stagesByPipeline = includeStages
                ? loadStagesForPipelines(pipelines, false)
                : Collections.emptyMap();

        return pipelines.stream()
                .map(p -> PipelineMapper.toPipelineResponse(
                        p,
                        stagesByPipeline.getOrDefault(p.getId(), Collections.emptyList()),
                        includeStages
                ))
                .collect(Collectors.toList());
    }

    @Override
    @Transactional(readOnly = true)
    public PipelineDtos.PipelineResponse getPipeline(Long pipelineId, boolean includeStages) {
        Pipeline pipeline = requireActivePipeline(pipelineId);
        List<Stage> stages = includeStages
                ? stageRepository.findByPipelineOrderByOrderIndexAsc(pipeline)
                : Collections.emptyList();
        return PipelineMapper.toPipelineResponse(pipeline, stages, includeStages);
    }

    @Override
    public PipelineDtos.PipelineResponse updatePipeline(Long pipelineId, PipelineDtos.PipelineUpdateRequest request) {
        Pipeline pipeline = requireActivePipeline(pipelineId);

        if (StringUtils.hasText(request.getName())) {
            validatePipelineName(request.getName(), pipelineId);
            pipeline.setName(request.getName().trim());
        }
        if (request.getCategory() != null) pipeline.setCategory(trimToNull(request.getCategory()));
        if (request.getTeamId() != null) pipeline.setTeam(resolveTeam(request.getTeamId()));
        if (request.getOrganizationId() != null) pipeline.setOrganization(resolveOrganization(request.getOrganizationId()));
        if (request.getDeleted() != null) pipeline.setDeleted(request.getDeleted());

        Pipeline saved = pipelineRepository.save(pipeline);
        List<Stage> stages = stageRepository.findByPipelineOrderByOrderIndexAsc(saved);
        return PipelineMapper.toPipelineResponse(saved, stages, true);
    }

    @Override
    public PipelineDtos.PipelineResponse unarchivePipeline(Long pipelineId, boolean includeStages) {
        Pipeline pipeline = requirePipeline(pipelineId);
        
        if (!Boolean.TRUE.equals(pipeline.getDeleted())) {
            throw new BadRequestException("Pipeline is not archived");
        }
        
        pipeline.setDeleted(false);
        Pipeline saved = pipelineRepository.save(pipeline);
        
        List<Stage> stages = includeStages
                ? stageRepository.findByPipelineOrderByOrderIndexAsc(saved)
                : Collections.emptyList();
        return PipelineMapper.toPipelineResponse(saved, stages, includeStages);
    }

    @Override
    public void deletePipeline(Long pipelineId, boolean hardDelete) {
        Pipeline pipeline = pipelineRepository.findById(pipelineId)
                .orElseThrow(() -> new ResourceNotFoundException("Pipeline not found"));

        if (hardDelete) {
            // Only check for non-deleted deals when validating pipeline deletion
            List<Deal> linkedDeals = dealRepository.findByPipelineAndIsDeletedFalse(pipeline);
            if (!linkedDeals.isEmpty()) {
                throw new BadRequestException("Cannot delete pipeline while " + linkedDeals.size()
                        + " deal(s) reference it. Reassign or delete those deals first.");
            }
            List<Stage> stages = stageRepository.findByPipelineOrderByOrderIndexAsc(pipeline);
            if (!stages.isEmpty()) {
                stageRepository.deleteAll(stages);
            }
            pipelineRepository.delete(pipeline);
        } else {
            pipeline.setDeleted(true);
            pipelineRepository.save(pipeline);
        }
    }

    @Override
    public PipelineDtos.StageResponse createStage(Long pipelineId, PipelineDtos.StageRequest request) {
        Pipeline pipeline = requireActivePipeline(pipelineId);
        validateStageRequest(pipeline, request.getName(), null);

        List<Stage> stages = stageRepository.findByPipelineOrderByOrderIndexAsc(pipeline);
        int targetOrder = resolveStageInsertOrder(request.getOrder(), stages.size());
        shiftStagesForInsert(stages, targetOrder);

        Stage stage = new Stage();
        stage.setPipeline(pipeline);
        stage.setName(request.getName().trim());
        stage.setOrderIndex(targetOrder);
        stage.setProbability(request.getProbability());
        stage.setActive(request.getActive() == null || request.getActive());

        Stage saved = stageRepository.save(stage);
        return PipelineMapper.toStageResponse(saved);
    }

    @Override
    public PipelineDtos.StageResponse updateStage(Long pipelineId, Long stageId, PipelineDtos.StageUpdateRequest request) {
        Pipeline pipeline = requireActivePipeline(pipelineId);
        Stage stage = stageRepository.findByIdAndPipeline(stageId, pipeline)
                .orElseThrow(() -> new ResourceNotFoundException("Stage not found in pipeline"));

        if (StringUtils.hasText(request.getName())) {
            validateStageRequest(pipeline, request.getName(), stageId);
            stage.setName(request.getName().trim());
        }
        if (request.getProbability() != null) {
            stage.setProbability(request.getProbability());
        }
        if (request.getActive() != null) {
            stage.setActive(request.getActive());
        }
        if (request.getOrder() != null) {
            repositionStage(pipeline, stage, request.getOrder());
        }

        Stage saved = stageRepository.save(stage);
        return PipelineMapper.toStageResponse(saved);
    }

    @Override
    public void deleteStage(Long pipelineId, Long stageId, boolean hardDelete) {
        Pipeline pipeline = requireActivePipeline(pipelineId);
        Stage stage = stageRepository.findByIdAndPipeline(stageId, pipeline)
                .orElseThrow(() -> new ResourceNotFoundException("Stage not found in pipeline"));

        if (hardDelete) {
            stageRepository.delete(stage);
        } else {
            stage.setActive(false);
            stageRepository.save(stage);
        }
        normalizeStageOrder(pipeline);
    }

    @Override
    @Transactional(readOnly = true)
    public List<PipelineDtos.StageResponse> listStages(Long pipelineId, boolean includeInactive) {
        Pipeline pipeline = requireActivePipeline(pipelineId);
        List<Stage> stages = includeInactive
                ? stageRepository.findByPipelineOrderByOrderIndexAsc(pipeline)
                : stageRepository.findByPipelineAndActiveTrueOrderByOrderIndexAsc(pipeline);
        return stages.stream()
                .map(PipelineMapper::toStageResponse)
                .collect(Collectors.toList());
    }

    @Override
    public List<PipelineDtos.StageResponse> reorderStages(Long pipelineId, PipelineDtos.StageOrderRequest request) {
        Pipeline pipeline = requireActivePipeline(pipelineId);

        List<Long> orderedStageIds = request.getOrderedStageIds();
        if (orderedStageIds == null || orderedStageIds.isEmpty()) {
            throw new BadRequestException("Stage order list cannot be empty");
        }

        List<Stage> stages = stageRepository.findByPipelineOrderByOrderIndexAsc(pipeline);
        if (stages.size() != orderedStageIds.size()) {
            throw new BadRequestException("Stage order list must include all stages in the pipeline");
        }

        Map<Long, Stage> stageMap = stages.stream()
                .collect(Collectors.toMap(Stage::getId, s -> s));

        Set<Long> uniqueIds = new HashSet<>(orderedStageIds);
        if (uniqueIds.size() != orderedStageIds.size()) {
            throw new BadRequestException("Duplicate stage ids supplied in order list");
        }

        for (Long stageId : orderedStageIds) {
            if (!stageMap.containsKey(stageId)) {
                throw new BadRequestException("Stage id " + stageId + " is not part of pipeline " + pipelineId);
            }
        }

        for (int i = 0; i < orderedStageIds.size(); i++) {
            Stage stage = stageMap.get(orderedStageIds.get(i));
            stage.setOrderIndex(i);
        }
        stageRepository.saveAll(stages);

        stages.sort(Comparator.comparing(Stage::getOrderIndex));
        return stages.stream()
                .map(PipelineMapper::toStageResponse)
                .collect(Collectors.toList());
    }

    private Organization resolveOrganization(Long organizationId) {
        if (organizationId == null) {
            return null;
        }
        return organizationRepository.findById(organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Organization not found with id " + organizationId));
    }

    private void validatePipelineName(String name, Long pipelineId) {
        if (!StringUtils.hasText(name)) {
            throw new BadRequestException("Pipeline name is required");
        }
        String trimmed = name.trim();
        boolean exists = pipelineId == null
                ? pipelineRepository.existsByNameIgnoreCase(trimmed)
                : pipelineRepository.existsByNameIgnoreCaseAndIdNot(trimmed, pipelineId);
        if (exists) {
            throw new BadRequestException("A pipeline with the same name already exists");
        }
    }

    private void validateStageRequest(Pipeline pipeline, String name, Long stageId) {
        if (!StringUtils.hasText(name)) {
            throw new BadRequestException("Stage name is required");
        }
        String trimmed = name.trim();
        boolean exists = stageId == null
                ? stageRepository.existsByPipelineAndNameIgnoreCase(pipeline, trimmed)
                : stageRepository.existsByPipelineAndNameIgnoreCaseAndIdNot(pipeline, trimmed, stageId);
        if (exists) {
            throw new BadRequestException("Stage name already exists in pipeline");
        }
    }

    private int resolveStageInsertOrder(Integer requestedOrder, int currentSize) {
        if (requestedOrder == null) {
            return currentSize;
        }
        if (requestedOrder < 0) {
            throw new BadRequestException("Stage order cannot be negative");
        }
        return Math.min(requestedOrder, currentSize);
    }

    private void shiftStagesForInsert(List<Stage> stages, int targetOrder) {
        for (Stage existing : stages) {
            if (existing.getOrderIndex() >= targetOrder) {
                existing.setOrderIndex(existing.getOrderIndex() + 1);
            }
        }
        if (!stages.isEmpty()) {
            stageRepository.saveAll(stages);
        }
    }

    private void repositionStage(Pipeline pipeline, Stage stage, int desiredOrder) {
        if (desiredOrder < 0) {
            throw new BadRequestException("Stage order cannot be negative");
        }
        List<Stage> stages = stageRepository.findByPipelineOrderByOrderIndexAsc(pipeline);
        stages.removeIf(s -> s.getId().equals(stage.getId()));

        int boundedOrder = Math.min(desiredOrder, stages.size());
        stages.add(boundedOrder, stage);

        for (int i = 0; i < stages.size(); i++) {
            stages.get(i).setOrderIndex(i);
        }
        stageRepository.saveAll(stages);
    }

    private void normalizeStageOrder(Pipeline pipeline) {
        List<Stage> stages = stageRepository.findByPipelineOrderByOrderIndexAsc(pipeline);
        for (int i = 0; i < stages.size(); i++) {
            stages.get(i).setOrderIndex(i);
        }
        if (!stages.isEmpty()) {
            stageRepository.saveAll(stages);
        }
    }

    private Map<Long, List<Stage>> loadStagesForPipelines(List<Pipeline> pipelines, boolean includeInactive) {
        Map<Long, List<Stage>> map = new HashMap<>();
        for (Pipeline pipeline : pipelines) {
            List<Stage> stages = includeInactive
                    ? stageRepository.findByPipelineOrderByOrderIndexAsc(pipeline)
                    : stageRepository.findByPipelineAndActiveTrueOrderByOrderIndexAsc(pipeline);
            map.put(pipeline.getId(), stages);
        }
        return map;
    }

    private Pipeline requireActivePipeline(Long pipelineId) {
        Pipeline pipeline = pipelineRepository.findById(pipelineId)
                .orElseThrow(() -> new ResourceNotFoundException("Pipeline not found"));
        if (Boolean.TRUE.equals(pipeline.getDeleted())) {
            throw new ResourceNotFoundException("Pipeline not found");
        }
        return pipeline;
    }

    private Pipeline requirePipeline(Long pipelineId) {
        return pipelineRepository.findById(pipelineId)
                .orElseThrow(() -> new ResourceNotFoundException("Pipeline not found"));
    }

    private String trimToNull(String value) {
        if (!StringUtils.hasText(value)) {
            return null;
        }
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    private Team resolveTeam(Long teamId) {
        if (teamId == null) {
            return null;
        }
        return teamRepository.findById(teamId)
                .orElseThrow(() -> new BadRequestException("Team not found with id " + teamId));
    }

    private List<Stage> createDefaultStages(Pipeline pipeline) {
        String[] defaultNames = {
                "Lead In",
                "Qualified",
                "Contact Made",
                "Follow Up",
                "Meeting Scheduled",
                "Contract Shared",
                "Diversion"
        };
        List<Stage> stages = new ArrayList<>();
        for (int i = 0; i < defaultNames.length; i++) {
            Stage stage = new Stage();
            stage.setPipeline(pipeline);
            stage.setName(defaultNames[i]);
            stage.setOrderIndex(i);
            stage.setActive(true);
            stage.setProbability(null);
            stages.add(stage);
        }
        return stageRepository.saveAll(stages);
    }

    @Override
    @Transactional(readOnly = true)
    public List<PipelineDtos.CategoryOption> listCategoryOptions() {
        return PipelineDtos.allCategoryOptions();
    }

    /**
     * Get the current logged-in user from SecurityContext
     */
    private Optional<User> getCurrentUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !(authentication.getPrincipal() instanceof UserDetails)) {
            return Optional.empty();
        }
        String email = ((UserDetails) authentication.getPrincipal()).getUsername();
        return userRepository.findByEmail(email);
    }

    /**
     * Get filtered pipelines based on the current user's role
     * Visibility rules:
     * 1. Admin: Sees all pipelines
     * 2. Category Manager: Sees pipelines linked to teams where their Sales Managers (users who report to them) are team managers
     * 3. Sales Manager (SALES role): Sees pipelines linked to teams they manage
     * 4. Pre-Sales (PRESALES role): Sees pipelines linked to teams where their Sales Manager (their manager) is the team manager
     */
    private List<Pipeline> getFilteredPipelines() {
        Optional<User> currentUserOpt = getCurrentUser();
        
        // If no user is logged in, return empty list (or all pipelines - depends on security requirements)
        // For now, returning empty list for unauthenticated users
        if (currentUserOpt.isEmpty() || currentUserOpt.get().getRole() == null) {
            return Collections.emptyList();
        }

        User currentUser = currentUserOpt.get();
        Role.RoleName roleName = currentUser.getRole().getName();

        // Admin sees all pipelines
        if (roleName == Role.RoleName.ADMIN) {
            return pipelineRepository.findByDeletedFalseOrderByNameAsc();
        }

        // Get team IDs based on role
        Set<Long> allowedTeamIds = getAllowedTeamIds(currentUser, roleName);
        
        // If no teams are allowed, return empty list
        if (allowedTeamIds.isEmpty()) {
            return Collections.emptyList();
        }

        // Return pipelines linked to allowed teams
        return pipelineRepository.findByDeletedFalseAndTeam_IdInOrderByNameAsc(new ArrayList<>(allowedTeamIds));
    }

    /**
     * Get allowed team IDs based on user role
     */
    private Set<Long> getAllowedTeamIds(User currentUser, Role.RoleName roleName) {
        Set<Long> teamIds = new HashSet<>();

        if (roleName == Role.RoleName.CATEGORY_MANAGER) {
            // Category Manager: Find all Sales users reporting to them
            // Then find teams where those Sales users are managers
            List<User> allUsers = userRepository.findAll();
            Set<Long> salesManagerIds = new HashSet<>();
            
            for (User user : allUsers) {
                if (user.getId() == null || user.getRole() == null) continue;
                
                // Direct reports (Sales users directly under Category Manager)
                if (user.getManager() != null && 
                    currentUser.getId().equals(user.getManager().getId()) &&
                    user.getRole().getName() == Role.RoleName.SALES) {
                    salesManagerIds.add(user.getId());
                }
            }
            
            // Find teams where these Sales Managers are team managers
            for (Long salesManagerId : salesManagerIds) {
                List<Team> teams = teamRepository.findByManager_Id(salesManagerId);
                for (Team team : teams) {
                    if (team.getId() != null) {
                        teamIds.add(team.getId());
                    }
                }
            }
        } else if (roleName == Role.RoleName.SALES) {
            // Sales Manager: Find teams where they are the team manager
            List<Team> teams = teamRepository.findByManager_Id(currentUser.getId());
            for (Team team : teams) {
                if (team.getId() != null) {
                    teamIds.add(team.getId());
                }
            }
        } else if (roleName == Role.RoleName.PRESALES) {
            // Pre-Sales: Find their Sales Manager (their manager)
            // Then find teams where that Sales Manager is the team manager
            if (currentUser.getManager() != null && currentUser.getManager().getId() != null) {
                Long salesManagerId = currentUser.getManager().getId();
                List<Team> teams = teamRepository.findByManager_Id(salesManagerId);
                for (Team team : teams) {
                    if (team.getId() != null) {
                        teamIds.add(team.getId());
                    }
                }
            }
        }

        return teamIds;
    }
}


