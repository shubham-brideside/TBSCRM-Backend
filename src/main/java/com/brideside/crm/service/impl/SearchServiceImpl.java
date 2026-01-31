package com.brideside.crm.service.impl;

import com.brideside.crm.dto.DealResponse;
import com.brideside.crm.dto.LabelDtos;
import com.brideside.crm.dto.PersonDTO;
import com.brideside.crm.dto.SearchDtos;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.Person;
import com.brideside.crm.mapper.PersonMapper;
import com.brideside.crm.repository.DealRepository;
import com.brideside.crm.repository.DealSpecifications;
import com.brideside.crm.repository.PersonRepository;
import com.brideside.crm.repository.PersonSpecifications;
import com.brideside.crm.service.SearchService;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class SearchServiceImpl implements SearchService {
    
    private static final Logger log = LoggerFactory.getLogger(SearchServiceImpl.class);
    private static final int DEFAULT_LIMIT = 10;
    
    private final PersonRepository personRepository;
    private final DealRepository dealRepository;
    private final ObjectMapper objectMapper = new ObjectMapper();
    
    public SearchServiceImpl(PersonRepository personRepository,
                            DealRepository dealRepository) {
        this.personRepository = personRepository;
        this.dealRepository = dealRepository;
    }
    
    @Override
    public SearchDtos.GlobalSearchResponse globalSearch(String query, Integer limit) {
        if (query == null || query.trim().isEmpty()) {
            return new SearchDtos.GlobalSearchResponse(List.of(), List.of());
        }
        
        int searchLimit = (limit != null && limit > 0) ? limit : DEFAULT_LIMIT;
        Pageable pageable = PageRequest.of(0, searchLimit);
        
        log.debug("Global search requested with query: '{}', limit: {}", query, searchLimit);
        
        // Search persons - use focusedSearch to only match name, instagramId, and phone
        // This avoids unrelated results from owner name, organization name, or email
        Specification<Person> personSpec = Specification.where(PersonSpecifications.notDeleted())
                .and(PersonSpecifications.focusedSearch(query));
        
        List<PersonDTO> persons = personRepository.findAll(personSpec, pageable)
                .stream()
                .map(PersonMapper::toDto)
                .collect(Collectors.toList());
        
        log.debug("Found {} persons matching query '{}'", persons.size(), query);
        
        // Search deals - use focusedSearch to only match deal name, phone number, and related person fields
        // This avoids unrelated results from venue or organization name
        Specification<Deal> dealSpec = Specification.where(DealSpecifications.notDeleted())
                .and(DealSpecifications.focusedSearch(query));
        
        List<Deal> dealEntities = dealRepository.findAll(dealSpec, pageable).getContent();
        
        // Convert deals to DealResponse using DealService's toResponse method
        // We need to use DealController's toResponse method, but since it's private,
        // we'll create a simpler conversion here or use DealService
        List<DealResponse> deals = dealEntities.stream()
                .map(this::toDealResponse)
                .collect(Collectors.toList());
        
        log.debug("Found {} deals matching query '{}'", deals.size(), query);
        
        return new SearchDtos.GlobalSearchResponse(persons, deals);
    }
    
    /**
     * Convert Deal entity to DealResponse
     * Similar to DealController.toResponse but adapted for search results
     */
    private DealResponse toDealResponse(Deal deal) {
        DealResponse response = new DealResponse();
        response.id = deal.getId();
        response.name = deal.getName();
        response.value = deal.getValue();
        response.personId = deal.getPerson() != null ? deal.getPerson().getId() : null;
        response.personName = deal.getPerson() != null ? deal.getPerson().getName() : null;
        response.pipelineId = deal.getPipeline() != null ? deal.getPipeline().getId() : null;
        response.stageId = deal.getStage() != null ? deal.getStage().getId() : null;
        response.sourceId = deal.getSource() != null ? deal.getSource().getId() : null;
        response.organizationId = deal.getOrganization() != null ? deal.getOrganization().getId() : null;
        response.organizationName = deal.getOrganization() != null ? deal.getOrganization().getName() : null;
        response.categoryId = deal.getDealCategory() != null ? deal.getDealCategory().getId() : null;
        response.eventType = deal.getEventType();
        response.status = deal.getStatus();
        response.commissionAmount = deal.getCommissionAmount();
        response.createdAt = deal.getCreatedAt();
        response.updatedAt = deal.getUpdatedAt();
        response.venue = deal.getVenue();
        response.phoneNumber = deal.getPhoneNumber();
        response.city = deal.getCity();
        response.finalThankYouSent = deal.getFinalThankYouSent();
        response.eventDateAsked = deal.getEventDateAsked();
        response.contactNumberAsked = deal.getContactNumberAsked();
        response.venueAsked = deal.getVenueAsked();
        response.eventDate = deal.getEventDate() != null ? deal.getEventDate().toString() : null;
        response.eventDates = parseEventDates(deal);
        response.isDeleted = deal.getIsDeleted();
        response.createdBy = deal.getCreatedBy();
        response.createdByUserId = deal.getCreatedByUserId();
        response.createdByName = deal.getCreatedByName();
        response.isDiverted = deal.getIsDiverted();
        response.referencedDealId = deal.getReferencedDeal() != null ? deal.getReferencedDeal().getId() : null;
        response.referencedPipelineId = deal.getReferencedPipeline() != null ? deal.getReferencedPipeline().getId() : null;
        response.sourcePipelineId = deal.getSourcePipeline() != null ? deal.getSourcePipeline().getId() : null;
        response.pipelineHistory = deal.getPipelineHistory();
        response.lostReason = deal.getLostReason() != null ? deal.getLostReason().toDisplayString() : null;
        response.clientBudget = deal.getClientBudget();
        
        // Handle source
        if (deal.getDealSource() != null) {
            response.source = deal.getDealSource().toDisplayString();
        }
        if (deal.getDealSubSource() != null) {
            response.subSource = deal.getDealSubSource().toDisplayString();
        }
        
        // Handle labels
        Set<com.brideside.crm.entity.Label> dealLabels = deal.getLabels();
        if (dealLabels != null && !dealLabels.isEmpty()) {
            List<Long> labelIdList = new ArrayList<>();
            List<LabelDtos.Response> labelList = new ArrayList<>();
            for (com.brideside.crm.entity.Label dealLabel : dealLabels) {
                dealLabel.getId(); // Trigger lazy load
                labelIdList.add(dealLabel.getId());
                labelList.add(new LabelDtos.Response(
                    dealLabel.getId(),
                    dealLabel.getName(),
                    dealLabel.getColor(),
                    dealLabel.getCreatedAt(),
                    dealLabel.getUpdatedAt()
                ));
            }
            response.labelIds = labelIdList;
            response.labels = labelList;
            if (!labelIdList.isEmpty()) {
                response.labelId = labelIdList.get(0);
                response.label = labelList.get(0);
            } else {
                response.labelId = null;
                response.label = null;
            }
            response.labelString = null;
        } else if (deal.getLabelEnum() != null) {
            response.labelString = deal.getLabelEnum().toDisplayString();
            response.labelId = null;
            response.label = null;
            response.labelIds = null;
            response.labels = null;
        } else {
            response.labelString = null;
            response.labelId = null;
            response.label = null;
            response.labelIds = null;
            response.labels = null;
        }
        
        return response;
    }
    
    /**
     * Parses eventDates JSON string to a list of date strings.
     */
    private List<String> parseEventDates(Deal deal) {
        if (deal.getEventDates() == null || deal.getEventDates().isEmpty()) {
            if (deal.getEventDate() != null) {
                return List.of(deal.getEventDate().toString());
            }
            return null;
        }
        try {
            return objectMapper.readValue(
                deal.getEventDates(),
                new TypeReference<List<String>>() {}
            );
        } catch (Exception e) {
            if (deal.getEventDate() != null) {
                return List.of(deal.getEventDate().toString());
            }
            return null;
        }
    }
}

