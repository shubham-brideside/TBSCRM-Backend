package com.brideside.crm.service.impl;

import com.brideside.crm.dto.CategoryDtos;
import com.brideside.crm.dto.PipelineDtos;
import com.brideside.crm.entity.*;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.mapper.PipelineMapper;
import com.brideside.crm.repository.*;
import com.brideside.crm.service.CategoryService;
import com.brideside.crm.service.DealAccessScope;
import com.brideside.crm.service.PipelineAccessService;
import com.brideside.crm.service.PipelineService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@Transactional
public class CategoryServiceImpl implements CategoryService {

    private final CategoryRepository categoryRepository;
    private final PipelineRepository pipelineRepository;
    private final DealRepository dealRepository;
    private final PersonRepository personRepository;
    private final UserRepository userRepository;
    private final StageRepository stageRepository;
    private final PipelineService pipelineService;
    private final PipelineAccessService pipelineAccessService;

    public CategoryServiceImpl(
            CategoryRepository categoryRepository,
            PipelineRepository pipelineRepository,
            DealRepository dealRepository,
            PersonRepository personRepository,
            UserRepository userRepository,
            StageRepository stageRepository,
            PipelineService pipelineService,
            PipelineAccessService pipelineAccessService) {
        this.categoryRepository = categoryRepository;
        this.pipelineRepository = pipelineRepository;
        this.dealRepository = dealRepository;
        this.personRepository = personRepository;
        this.userRepository = userRepository;
        this.stageRepository = stageRepository;
        this.pipelineService = pipelineService;
        this.pipelineAccessService = pipelineAccessService;
    }

    @Override
    @Transactional(readOnly = true)
    public List<CategoryDtos.ManagerResponse> getManagersByCategory(String categoryIdentifier) {
        // Resolve category name from identifier (ID or name)
        String categoryName = resolveCategoryName(categoryIdentifier);
        Long categoryId = resolveCategoryId(categoryIdentifier);
        
        Set<Long> managerIds = new HashSet<>();
        
        // 1. Find team managers of pipelines with matching category
        List<Pipeline> pipelines = pipelineRepository.findByDeletedFalseAndCategoryOrderByNameAsc(categoryName);
        for (Pipeline pipeline : pipelines) {
            if (pipeline.getTeam() != null && pipeline.getTeam().getManager() != null) {
                Long managerId = pipeline.getTeam().getManager().getId();
                if (managerId != null) {
                    managerIds.add(managerId);
                }
            }
        }
        
        // 2. Find person owners of deals with matching category
        // Deals can have categoryId or be in pipelines with matching category
        if (categoryId != null) {
            // Find deals by categoryId
            Category category = categoryRepository.findById(categoryId)
                    .orElse(null);
            if (category != null) {
                List<Deal> dealsByCategory = dealRepository.findByDealCategoryAndIsDeletedFalse(category);
                for (Deal deal : dealsByCategory) {
                    if (deal.getPerson() != null && deal.getPerson().getOwner() != null) {
                        Long ownerId = deal.getPerson().getOwner().getId();
                        if (ownerId != null) {
                            managerIds.add(ownerId);
                        }
                    }
                }
            }
        }
        
        // Also check deals in pipelines with matching category
        for (Pipeline pipeline : pipelines) {
            List<Deal> dealsInPipeline = dealRepository.findByPipelineAndIsDeletedFalse(pipeline);
            for (Deal deal : dealsInPipeline) {
                if (deal.getPerson() != null && deal.getPerson().getOwner() != null) {
                    Long ownerId = deal.getPerson().getOwner().getId();
                    if (ownerId != null) {
                        managerIds.add(ownerId);
                    }
                }
            }
        }
        
        // Fetch all managers and filter by role (SALES or CATEGORY_MANAGER)
        if (managerIds.isEmpty()) {
            return Collections.emptyList();
        }
        
        List<User> managers = userRepository.findAllById(managerIds);
        // Use a LinkedHashMap to preserve order while removing duplicates by ID
        Map<Long, CategoryDtos.ManagerResponse> managerMap = new LinkedHashMap<>();
        managers.stream()
                .filter(user -> user.getRole() != null && 
                        (user.getRole().getName() == Role.RoleName.SALES || 
                         user.getRole().getName() == Role.RoleName.CATEGORY_MANAGER ||
                         user.getRole().getName() == Role.RoleName.ADMIN))
                .filter(user -> Boolean.TRUE.equals(user.getActive()))
                .forEach(user -> managerMap.putIfAbsent(user.getId(), toManagerResponse(user)));
        
        return new ArrayList<>(managerMap.values());
    }

    @Override
    @Transactional(readOnly = true)
    public List<PipelineDtos.PipelineResponse> getPipelinesByCategory(String categoryIdentifier, boolean includeStages) {
        // Resolve category name from identifier
        String categoryName = resolveCategoryName(categoryIdentifier);
        
        // Get all pipelines with this category
        List<Pipeline> allPipelines = pipelineRepository.findByDeletedFalseAndCategoryOrderByNameAsc(categoryName);
        
        // Filter by user permissions using the same logic as PipelineService
        List<Pipeline> filteredPipelines = filterPipelinesByUserPermissions(allPipelines);
        
        // Load stages if requested
        Map<Long, List<Stage>> stagesByPipeline = includeStages
                ? loadStagesForPipelines(filteredPipelines, false)
                : Collections.emptyMap();
        
        return filteredPipelines.stream()
                .map(p -> PipelineMapper.toPipelineResponse(
                        p,
                        stagesByPipeline.getOrDefault(p.getId(), Collections.emptyList()),
                        includeStages
                ))
                .collect(Collectors.toList());
    }

    /**
     * Resolve category name from identifier (can be ID or name)
     */
    private String resolveCategoryName(String categoryIdentifier) {
        // Try to parse as Long (category ID)
        try {
            Long categoryId = Long.parseLong(categoryIdentifier);
            Optional<Category> category = categoryRepository.findById(categoryId);
            if (category.isPresent()) {
                return category.get().getName();
            }
        } catch (NumberFormatException e) {
            // Not a number, treat as category name
        }
        
        // Try to find by name (case-insensitive)
        Optional<Category> category = categoryRepository.findByNameIgnoreCase(categoryIdentifier);
        if (category.isPresent()) {
            return category.get().getName();
        }
        
        // If not found in database, assume it's a category name from PipelineDtos.allCategoryOptions()
        // Map common category codes to names
        Map<String, String> categoryMap = Map.ofEntries(
                Map.entry("PHOTOGRAPHY", "Photography"),
                Map.entry("MAKEUP", "Makeup"),
                Map.entry("PLANNING", "Planning"),
                Map.entry("DECOR", "Decor"),
                Map.entry("BTS", "BTS"),
                Map.entry("PLANNING_AND_DECOR", "Planning and Decor")
        );
        
        if (categoryMap.containsKey(categoryIdentifier.toUpperCase())) {
            return categoryMap.get(categoryIdentifier.toUpperCase());
        }
        
        // Return as-is (might be a direct category name)
        return categoryIdentifier;
    }

    /**
     * Resolve category ID from identifier (can be ID or name)
     */
    private Long resolveCategoryId(String categoryIdentifier) {
        // Try to parse as Long (category ID)
        try {
            Long categoryId = Long.parseLong(categoryIdentifier);
            if (categoryRepository.existsById(categoryId)) {
                return categoryId;
            }
        } catch (NumberFormatException e) {
            // Not a number, treat as category name
        }
        
        // Try to find by name (case-insensitive)
        Optional<Category> category = categoryRepository.findByNameIgnoreCase(categoryIdentifier);
        if (category.isPresent()) {
            return category.get().getId();
        }
        
        return null;
    }

    /**
     * Filter pipelines by the same visibility rules as {@link com.brideside.crm.service.PipelineAccessService}.
     */
    private List<Pipeline> filterPipelinesByUserPermissions(List<Pipeline> pipelines) {
        DealAccessScope scope = pipelineAccessService.resolveDealAccessScope();
        if (scope.fullAccess()) {
            return pipelines;
        }
        if (scope.allowedPipelineIds().isEmpty()) {
            return Collections.emptyList();
        }
        Set<Long> allowed = scope.allowedPipelineIds();
        return pipelines.stream()
                .filter(p -> p.getId() != null && allowed.contains(p.getId()))
                .collect(Collectors.toList());
    }

    /**
     * Convert User to ManagerResponse
     */
    private CategoryDtos.ManagerResponse toManagerResponse(User user) {
        String displayName = user.getFirstName() + " " + user.getLastName();
        String role = user.getRole() != null ? user.getRole().getName().name() : null;
        return new CategoryDtos.ManagerResponse(
                user.getId(),
                user.getEmail(),
                displayName,
                role
        );
    }

    /**
     * Load stages for pipelines
     */
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
}

