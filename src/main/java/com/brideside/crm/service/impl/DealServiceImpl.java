package com.brideside.crm.service.impl;

import com.brideside.crm.dto.DealDtos;
import com.brideside.crm.dto.PipelineDtos;
import com.brideside.crm.entity.Category;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.DealLabel;
import com.brideside.crm.entity.DealSource;
import com.brideside.crm.entity.DealStatus;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Person;
import com.brideside.crm.entity.Pipeline;
import com.brideside.crm.entity.Source;
import com.brideside.crm.entity.Stage;
import com.brideside.crm.mapper.PipelineMapper;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.CategoryRepository;
import com.brideside.crm.repository.DealRepository;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.repository.PersonRepository;
import com.brideside.crm.repository.PipelineRepository;
import com.brideside.crm.repository.SourceRepository;
import com.brideside.crm.repository.StageRepository;
import com.brideside.crm.service.DealService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

@Service
public class DealServiceImpl implements DealService {

    private static final Logger log = LoggerFactory.getLogger(DealServiceImpl.class);

    @Autowired private DealRepository dealRepository;
    @Autowired private PersonRepository personRepository;
    @Autowired private PipelineRepository pipelineRepository;
    @Autowired private StageRepository stageRepository;
    @Autowired private SourceRepository sourceRepository;
    @Autowired private OrganizationRepository organizationRepository;
    @Autowired private CategoryRepository categoryRepository;
    
    private final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    public Deal create(DealDtos.CreateRequest request) {
        Deal deal = new Deal();
        deal.setName(request.name);
        
        // Don't save deal value when creating a diverted deal
        boolean isDivertedDeal = request.label != null && 
            DealLabel.fromString(request.label) == DealLabel.DIVERT;
        
        if (isDivertedDeal) {
            deal.setValue(BigDecimal.ZERO); // Set to zero for diverted deals
        } else {
        deal.setValue(request.value != null ? request.value : BigDecimal.ZERO);
        }
        if (request.personId != null) {
            Person person = personRepository.findById(request.personId)
                .orElseThrow(() -> new ResourceNotFoundException("Person not found"));
            deal.setPerson(person);
            String phone = person.getPhone();
            if (phone != null) {
                deal.setContactNumber(phone);
            }
        }
        if (request.pipelineId != null) {
            Pipeline pipeline = pipelineRepository.findById(request.pipelineId)
                .orElseThrow(() -> new ResourceNotFoundException("Pipeline not found"));
            deal.setPipeline(pipeline);
        }
        if (request.stageId != null) {
            Stage stage = stageRepository.findById(request.stageId)
                .orElseThrow(() -> new ResourceNotFoundException("Stage not found"));
            deal.setStage(stage);
        }
        if (request.sourceId != null) {
            Source source = sourceRepository.findById(request.sourceId)
                .orElseThrow(() -> new ResourceNotFoundException("Source not found"));
            deal.setSource(source);
            deal.setCommissionAmount(calculateCommission(deal.getValue(), source));
        }
        if (request.organizationId != null) {
            Organization organization = organizationRepository.findById(request.organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Organization not found"));
            deal.setOrganization(organization);
        }
        Category selectedCategory = resolveSelectedCategory(request);
        if (selectedCategory != null) {
            deal.setDealCategory(selectedCategory);
        }
        deal.setEventType(request.eventType);
        if (request.status != null) {
            deal.setStatus(request.status);
        } else {
            deal.setStatus(DealStatus.IN_PROGRESS);
        }
        // Keep legacy 'won' column in sync for DBs that still require it
        deal.setLegacyWon(deal.getStatus() == DealStatus.WON);
        if (request.commissionAmount != null) {
            deal.setCommissionAmount(request.commissionAmount);
        }
        if (deal.getCategory() == null) {
            deal.setCategory("GENERAL");
        }
        if (deal.getContactNumber() == null) {
            deal.setContactNumber("");
        }
        if (deal.getUserName() == null) {
            deal.setUserName("");
        }
        if (deal.getCreatedAt() == null) {
            deal.setCreatedAt(LocalDateTime.now());
        }
        // Optional new fields
        deal.setVenue(request.venue);
        deal.setPhoneNumber(request.phoneNumber);
        deal.setFinalThankYouSent(request.finalThankYouSent);
        deal.setEventDateAsked(request.eventDateAsked);
        deal.setContactNumberAsked(request.contactNumberAsked);
        deal.setVenueAsked(request.venueAsked);
        if (request.eventDate != null && !request.eventDate.isEmpty()) {
            deal.setEventDate(java.time.LocalDate.parse(request.eventDate));
        }
        // Handle label field with validation
        if (request.label != null && !request.label.trim().isEmpty()) {
            DealLabel label = DealLabel.fromString(request.label);
            if (label == null) {
                throw new BadRequestException("Invalid label value: " + request.label + 
                    ". Allowed values: DIRECT, DIVERT, DESTINATION, PARTY MAKEUP, PRE WEDDING");
            }
            deal.setLabel(label);
            
            // If label is DIVERT, set is_diverted to true
            if (label == DealLabel.DIVERT) {
                deal.setIsDiverted(Boolean.TRUE);
                // Validate that referencedDealId is provided when diverting
                if (request.referencedDealId == null) {
                    throw new BadRequestException("referencedDealId is required when label is DIVERT");
                }
            }
        }
        
        // Handle referenced deal (for diversion)
        if (request.referencedDealId != null) {
            Deal referencedDeal = dealRepository.findById(request.referencedDealId)
                .orElseThrow(() -> new ResourceNotFoundException("Referenced deal not found with id " + request.referencedDealId));
            
            // Check if referenced deal is deleted
            if (referencedDeal.getIsDeleted() != null && referencedDeal.getIsDeleted()) {
                throw new ResourceNotFoundException("Referenced deal not found with id " + request.referencedDealId);
            }
            
            deal.setReferencedDeal(referencedDeal);
            
            // Set the referenced pipeline to the original pipeline (traverse up the chain if needed)
            Pipeline originalPipeline = getOriginalPipeline(referencedDeal);
            if (originalPipeline != null) {
                deal.setReferencedPipeline(originalPipeline);
            }
            
            // Track pipeline history for diversion prevention
            Pipeline sourcePipeline = getSourcePipeline(referencedDeal);
            if (sourcePipeline != null) {
                deal.setSourcePipeline(sourcePipeline);
            }
            
            // Build pipeline history: get history from referenced deal and add its current pipeline
            List<Long> pipelineHistory = getPipelineHistory(referencedDeal);
            if (referencedDeal.getPipeline() != null) {
                Long currentPipelineId = referencedDeal.getPipeline().getId();
                if (!pipelineHistory.contains(currentPipelineId)) {
                    pipelineHistory.add(currentPipelineId);
                }
            }
            deal.setPipelineHistory(pipelineHistoryToJson(pipelineHistory));
        }
        
        // For non-diverted deals, set source pipeline to current pipeline if not set
        if (request.referencedDealId == null && deal.getPipeline() != null) {
            deal.setSourcePipeline(deal.getPipeline());
            // Initialize pipeline history with current pipeline
            List<Long> initialHistory = new ArrayList<>();
            initialHistory.add(deal.getPipeline().getId());
            deal.setPipelineHistory(pipelineHistoryToJson(initialHistory));
        }
        
        // Handle source field with validation
        if (request.source != null && !request.source.trim().isEmpty()) {
            DealSource dealSource = DealSource.fromString(request.source);
            if (dealSource == null) {
                throw new BadRequestException("Invalid source value: " + request.source + 
                    ". Allowed values: Instagram, Whatsapp, Email, Reference, Call, Website");
            }
            deal.setDealSource(dealSource);
        }
        return dealRepository.save(deal);
    }

    @Override
    public Deal get(Long id) {
        Deal deal = dealRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Deal not found"));
        // Check if deal is deleted
        if (deal.getIsDeleted() != null && deal.getIsDeleted()) {
            throw new ResourceNotFoundException("Deal not found");
        }
        return deal;
    }

    @Override
    public List<Deal> list() { 
        return dealRepository.findByIsDeletedFalse(); 
    }

    @Override
    public List<Deal> listWon() { 
        return dealRepository.findByStatusAndIsDeletedFalse(DealStatus.WON); 
    }

    @Override
    public List<Deal> listByStatus(DealStatus status) { 
        return dealRepository.findByStatusAndIsDeletedFalse(status); 
    }

    @Override
    public List<Deal> listByPerson(Long personId) {
        Person person = personRepository.findById(personId)
            .orElseThrow(() -> new ResourceNotFoundException("Person not found"));
        return dealRepository.findByPersonAndIsDeletedFalse(person);
    }

    @Override
    public List<Deal> listByOrganization(Long organizationId) {
        Organization organization = organizationRepository.findById(organizationId)
            .orElseThrow(() -> new ResourceNotFoundException("Organization not found"));
        return dealRepository.findByOrganizationAndIsDeletedFalse(organization);
    }

    @Override
    public List<Deal> listByCategory(Long categoryId) {
        Category category = categoryRepository.findById(categoryId)
            .orElseThrow(() -> new ResourceNotFoundException("Category not found"));
        return dealRepository.findByDealCategoryAndIsDeletedFalse(category);
    }

    @Override
    public Deal updateStage(Long id, DealDtos.UpdateStageRequest request) {
        Deal deal = get(id);
        Stage stage = stageRepository.findById(request.stageId)
                .orElseThrow(() -> new ResourceNotFoundException("Stage not found"));
        deal.setStage(stage);
        Deal saved = dealRepository.save(deal);
        return saved;
    }

    @Override
    public Deal markStatus(Long id, DealStatus status) {
        Deal deal = get(id);
        deal.setStatus(status);
        // Sync legacy 'won' column
        deal.setLegacyWon(status == DealStatus.WON);
        if (status == DealStatus.WON && deal.getSource() != null && deal.getCommissionAmount() == null) {
            deal.setCommissionAmount(calculateCommission(deal.getValue(), deal.getSource()));
        }
        Deal saved = dealRepository.save(deal);
        syncGoogleCalendarEvent(saved);
        return saved;
    }

    @Override
    public void delete(Long id) {
        Deal deal = get(id);
        removeGoogleCalendarEvent(deal);
        dealRepository.delete(deal);
    }

    private Category resolveOrCreateCategory(Organization.OrganizationCategory orgCategory) {
        return categoryRepository.findByNameIgnoreCase(orgCategory.getDbValue())
                .orElseGet(() -> {
                    Category category = new Category();
                    category.setName(orgCategory.getDbValue());
                    return categoryRepository.save(category);
                });
    }

    private Category resolveSelectedCategory(DealDtos.CreateRequest request) {
        // Try to interpret categoryId first (may be numeric id or string code)
        if (request.categoryId != null && !request.categoryId.isBlank()) {
            String trimmed = request.categoryId.trim();
            try {
                Long id = Long.valueOf(trimmed);
                return categoryRepository.findById(id)
                        .orElseThrow(() -> new ResourceNotFoundException("Category not found with id " + id));
            } catch (NumberFormatException ex) {
                Organization.OrganizationCategory orgCategory = Organization.OrganizationCategory.fromDbValue(trimmed);
                if (orgCategory == null) {
                    throw new BadRequestException("Unknown category value: " + trimmed);
                }
                return resolveOrCreateCategory(orgCategory);
            }
        }

        if (request.category != null && !request.category.isBlank()) {
            Organization.OrganizationCategory orgCategory = Organization.OrganizationCategory.fromDbValue(request.category);
            if (orgCategory == null) {
                throw new BadRequestException("Unknown category value: " + request.category);
            }
            return resolveOrCreateCategory(orgCategory);
        }

        return null;
    }

    @Override
    @Transactional
    public void delete(Long id) {
        // Check if deal exists and is not already deleted
        Deal deal = dealRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Deal not found with id " + id));
        
        if (deal.getIsDeleted() != null && deal.getIsDeleted()) {
            throw new ResourceNotFoundException("Deal not found with id " + id);
        }
        
        // Find all deals that reference this deal (diverted deals) - only non-deleted ones
        List<Deal> referencingDeals = dealRepository.findByReferencedDealAndIsDeletedFalse(deal);
        
        // Clear the reference for all deals that reference this one
        // This prevents foreign key constraint violations
        for (Deal referencingDeal : referencingDeals) {
            referencingDeal.setReferencedDeal(null);
            // Also clear the referenced pipeline since the original deal is being deleted
            referencingDeal.setReferencedPipeline(null);
            dealRepository.save(referencingDeal);
        }
        
        // Soft delete: set is_deleted to true instead of hard delete
        deal.setIsDeleted(Boolean.TRUE);
        dealRepository.save(deal);
    }

    private Category resolveOrCreateCategory(Organization.OrganizationCategory orgCategory) {
        return categoryRepository.findByNameIgnoreCase(orgCategory.getDbValue())
                .orElseGet(() -> {
                    Category category = new Category();
                    category.setName(orgCategory.getDbValue());
                    return categoryRepository.save(category);
                });
    }

    private Category resolveSelectedCategory(DealDtos.CreateRequest request) {
        // Try to interpret categoryId first (may be numeric id or string code)
        if (request.categoryId != null && !request.categoryId.isBlank()) {
            String trimmed = request.categoryId.trim();
            try {
                Long id = Long.valueOf(trimmed);
                return categoryRepository.findById(id)
                        .orElseThrow(() -> new ResourceNotFoundException("Category not found with id " + id));
            } catch (NumberFormatException ex) {
                Organization.OrganizationCategory orgCategory = Organization.OrganizationCategory.fromDbValue(trimmed);
                if (orgCategory == null) {
                    throw new BadRequestException("Unknown category value: " + trimmed);
                }
                return resolveOrCreateCategory(orgCategory);
            }
        }

        if (request.category != null && !request.category.isBlank()) {
            Organization.OrganizationCategory orgCategory = Organization.OrganizationCategory.fromDbValue(request.category);
            if (orgCategory == null) {
                throw new BadRequestException("Unknown category value: " + request.category);
            }
            return resolveOrCreateCategory(orgCategory);
        }

        return null;
    }

    private BigDecimal calculateCommission(BigDecimal value, Source source) {
        if (value == null) return BigDecimal.ZERO;
        if (source.getFixedCommissionAmount() != null) return source.getFixedCommissionAmount();
        Integer pct = source.getCommissionPercentage() == null ? 0 : source.getCommissionPercentage();
        return value.multiply(BigDecimal.valueOf(pct)).divide(BigDecimal.valueOf(100));
    }

    /**
     * Gets the original pipeline from which a deal was diverted.
     * If the deal is already diverted, traverses up the chain to find the original pipeline.
     * If the deal is not diverted, returns its current pipeline.
     */
    private Pipeline getOriginalPipeline(Deal deal) {
        // If the deal has a referenced pipeline, it means it was diverted
        // Traverse up the chain to find the original pipeline
        if (deal.getReferencedPipeline() != null) {
            return deal.getReferencedPipeline();
        }
        
        // If the deal is not diverted, return its current pipeline
        return deal.getPipeline();
    }

    /**
     * Gets the source pipeline (initial pipeline) from which a deal was first created/diverted.
     * Traverses up the chain to find the source pipeline.
     */
    private Pipeline getSourcePipeline(Deal deal) {
        // If the deal has a source pipeline, return it
        if (deal.getSourcePipeline() != null) {
            return deal.getSourcePipeline();
        }
        
        // If not set, check if it's a diverted deal and get source from referenced deal
        if (deal.getReferencedDeal() != null) {
            return getSourcePipeline(deal.getReferencedDeal());
        }
        
        // If not diverted, return current pipeline as source
        return deal.getPipeline();
    }

    /**
     * Gets the pipeline history as a list of pipeline IDs.
     */
    private List<Long> getPipelineHistory(Deal deal) {
        List<Long> history = new ArrayList<>();
        
        // Skip deleted deals
        if (deal.getIsDeleted() != null && deal.getIsDeleted()) {
            return history;
        }
        
        // Get history from referenced deal if it exists and is not deleted
        if (deal.getReferencedDeal() != null) {
            Deal referencedDeal = deal.getReferencedDeal();
            if (referencedDeal.getIsDeleted() == null || !referencedDeal.getIsDeleted()) {
                history = getPipelineHistory(referencedDeal);
            }
        }
        
        // Parse JSON history if exists
        if (deal.getPipelineHistory() != null && !deal.getPipelineHistory().isEmpty()) {
            try {
                List<Long> jsonHistory = objectMapper.readValue(
                    deal.getPipelineHistory(),
                    new TypeReference<List<Long>>() {}
                );
                // Merge with existing history, avoiding duplicates
                for (Long pipelineId : jsonHistory) {
                    if (!history.contains(pipelineId)) {
                        history.add(pipelineId);
                    }
                }
            } catch (Exception e) {
                // If JSON parsing fails, continue with existing history
            }
        }
        
        return history;
    }

    /**
     * Converts a list of pipeline IDs to JSON string.
     */
    private String pipelineHistoryToJson(List<Long> pipelineIds) {
        try {
            return objectMapper.writeValueAsString(pipelineIds);
        } catch (Exception e) {
            return "[]";
        }
    }

    @Override
    public List<PipelineDtos.PipelineResponse> getAvailablePipelinesForDiversion(Long dealId) {
        // Get the deal (could be original or already diverted)
        Deal deal = dealRepository.findById(dealId)
                .orElseThrow(() -> new ResourceNotFoundException("Deal not found with id " + dealId));
        
        // Check if deal is deleted
        if (deal.getIsDeleted() != null && deal.getIsDeleted()) {
            throw new ResourceNotFoundException("Deal not found with id " + dealId);
        }
        
        // Get pipeline history - all pipelines this deal has been in
        List<Long> pipelineHistory = getPipelineHistory(deal);
        
        // Also add current pipeline to history if not already there
        if (deal.getPipeline() != null) {
            Long currentPipelineId = deal.getPipeline().getId();
            if (!pipelineHistory.contains(currentPipelineId)) {
                pipelineHistory.add(currentPipelineId);
            }
        }
        
        // Get all active pipelines
        List<Pipeline> allPipelines = pipelineRepository.findByDeletedFalseOrderByNameAsc();
        
        // Get all deals in the diversion chain (current deal + all deals it references)
        List<Deal> dealsInChain = getAllDealsInChain(deal);
        
        // Filter out:
        // 1. Pipelines where a diverted deal already exists for ANY deal in the chain
        // 2. All pipelines in the history (pipelines the deal has already been in)
        // 3. Pipelines where any deal diverted FROM this deal (or deals in chain) has been diverted to
        return allPipelines.stream()
                .filter(pipeline -> {
                    // Exclude all pipelines in history (prevents diverting back to any previous pipeline)
                    if (pipelineHistory.contains(pipeline.getId())) {
                        return false;
                    }
                    
                    // Exclude if ANY deal in the diversion chain has already been diverted to this pipeline (only non-deleted deals)
                    for (Deal dealInChain : dealsInChain) {
                        // Check if there's a non-deleted diverted deal
                        List<Deal> divertedDeals = dealRepository.findByReferencedDealAndIsDeletedFalse(dealInChain);
                        for (Deal divertedDeal : divertedDeals) {
                            if (divertedDeal.getPipeline() != null && divertedDeal.getPipeline().getId().equals(pipeline.getId())) {
                                return false;
                            }
                        }
                    }
                    
                    // Exclude if any deal that was diverted FROM deals in the chain has been diverted to this pipeline
                    // This handles the case: Pipeline1 → Pipeline2 → Pipeline3
                    // When checking from Pipeline1, we need to exclude Pipeline3 because Pipeline2 (diverted from Pipeline1) was diverted to Pipeline3
                    for (Deal dealInChain : dealsInChain) {
                        // Get all deals that were diverted FROM this deal (only non-deleted)
                        List<Deal> divertedDeals = dealRepository.findByReferencedDealAndIsDeletedFalse(dealInChain);
                        // Check if any of those diverted deals have been diverted to the target pipeline (only non-deleted)
                        for (Deal divertedDeal : divertedDeals) {
                            List<Deal> furtherDivertedDeals = dealRepository.findByReferencedDealAndIsDeletedFalse(divertedDeal);
                            for (Deal furtherDivertedDeal : furtherDivertedDeals) {
                                if (furtherDivertedDeal.getPipeline() != null && furtherDivertedDeal.getPipeline().getId().equals(pipeline.getId())) {
                                    return false;
                                }
                            }
                        }
                    }
                    
                    return true;
                })
                .map(pipeline -> PipelineMapper.toPipelineResponse(pipeline, Collections.emptyList(), false))
                .collect(Collectors.toList());
    }
    
    /**
     * Gets all deals in the diversion chain (current deal + all deals it references up to the original).
     * Returns a list starting from the current deal and going up to the original deal.
     */
    private List<Deal> getAllDealsInChain(Deal deal) {
        List<Deal> chain = new ArrayList<>();
        Deal current = deal;
        
        // Add current deal and traverse up the chain (only non-deleted deals)
        while (current != null) {
            // Only add non-deleted deals to the chain
            if (current.getIsDeleted() == null || !current.getIsDeleted()) {
                chain.add(current);
            }
            current = current.getReferencedDeal();
        }
        
        return chain;
    }
}




