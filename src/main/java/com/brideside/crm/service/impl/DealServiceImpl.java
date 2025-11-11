package com.brideside.crm.service.impl;

import com.brideside.crm.dto.DealDtos;
import com.brideside.crm.entity.*;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.*;
import com.brideside.crm.service.DealService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Service
public class DealServiceImpl implements DealService {

    @Autowired private DealRepository dealRepository;
    @Autowired private PersonRepository personRepository;
    @Autowired private PipelineRepository pipelineRepository;
    @Autowired private StageRepository stageRepository;
    @Autowired private SourceRepository sourceRepository;
    @Autowired private OrganizationRepository organizationRepository;
    @Autowired private CategoryRepository categoryRepository;

    @Override
    public Deal create(DealDtos.CreateRequest request) {
        Deal deal = new Deal();
        deal.setName(request.name);
        deal.setValue(request.value != null ? request.value : BigDecimal.ZERO);
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
        if (request.categoryId != null) {
            Category category = categoryRepository.findById(request.categoryId)
                .orElseThrow(() -> new ResourceNotFoundException("Category not found"));
            deal.setDealCategory(category);
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
        return dealRepository.save(deal);
    }

    @Override
    public Deal get(Long id) {
        return dealRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Deal not found"));
    }

    @Override
    public List<Deal> list() { return dealRepository.findAll(); }

    @Override
    public List<Deal> listWon() { return dealRepository.findByStatus(DealStatus.WON); }

    @Override
    public List<Deal> listByStatus(DealStatus status) { return dealRepository.findByStatus(status); }

    @Override
    public List<Deal> listByPerson(Long personId) {
        Person person = personRepository.findById(personId)
            .orElseThrow(() -> new ResourceNotFoundException("Person not found"));
        return dealRepository.findByPerson(person);
    }

    @Override
    public List<Deal> listByOrganization(Long organizationId) {
        Organization organization = organizationRepository.findById(organizationId)
            .orElseThrow(() -> new ResourceNotFoundException("Organization not found"));
        return dealRepository.findByOrganization(organization);
    }

    @Override
    public List<Deal> listByCategory(Long categoryId) {
        Category category = categoryRepository.findById(categoryId)
            .orElseThrow(() -> new ResourceNotFoundException("Category not found"));
        return dealRepository.findByDealCategory(category);
    }

    @Override
    public Deal updateStage(Long id, DealDtos.UpdateStageRequest request) {
        Deal deal = get(id);
        Stage stage = stageRepository.findById(request.stageId)
                .orElseThrow(() -> new ResourceNotFoundException("Stage not found"));
        deal.setStage(stage);
        return dealRepository.save(deal);
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
        return dealRepository.save(deal);
    }

    private BigDecimal calculateCommission(BigDecimal value, Source source) {
        if (value == null) return BigDecimal.ZERO;
        if (source.getFixedCommissionAmount() != null) return source.getFixedCommissionAmount();
        Integer pct = source.getCommissionPercentage() == null ? 0 : source.getCommissionPercentage();
        return value.multiply(BigDecimal.valueOf(pct)).divide(BigDecimal.valueOf(100));
    }
}


