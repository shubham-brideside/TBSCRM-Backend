package com.brideside.crm.service.impl;

import com.brideside.crm.dto.DealDtos;
import com.brideside.crm.dto.DealEditRequestDtos;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.DealEditRequest;
import com.brideside.crm.entity.DealEditRequestStatus;
import com.brideside.crm.entity.DealStatus;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.exception.UnauthorizedException;
import com.brideside.crm.repository.DealEditRequestRepository;
import com.brideside.crm.repository.DealRepository;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.DealEditRequestService;
import com.brideside.crm.service.DealService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

@Service
public class DealEditRequestServiceImpl implements DealEditRequestService {

    @Autowired
    private DealEditRequestRepository dealEditRequestRepository;

    @Autowired
    private DealRepository dealRepository;

    @Autowired
    private DealService dealService;

    @Autowired
    private UserRepository userRepository;

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Override
    @Transactional
    public DealEditRequest create(Long dealId, DealEditRequestDtos.CreateRequest request) {
        if (request == null || request.changes == null) {
            throw new BadRequestException("changes is required");
        }

        Deal deal = dealRepository.findById(dealId)
                .orElseThrow(() -> new ResourceNotFoundException("Deal not found"));

        if (deal.getStatus() != DealStatus.WON) {
            throw new BadRequestException("Edit requests are only allowed for WON deals");
        }

        User currentUser = getCurrentUser();

        DealEditRequest entity = new DealEditRequest();
        entity.setDeal(deal);
        entity.setRequestedBy(currentUser);
        entity.setReason(request.reason);

        try {
            String json = objectMapper.writeValueAsString(request.changes);
            entity.setRequestedChanges(json);
        } catch (JsonProcessingException e) {
            throw new BadRequestException("Invalid changes payload");
        }

        entity.setStatus(DealEditRequestStatus.PENDING);
        return dealEditRequestRepository.save(entity);
    }

    @Override
    @Transactional(readOnly = true)
    public List<DealEditRequest> listPending() {
        return dealEditRequestRepository.findByStatusOrderByCreatedAtDesc(DealEditRequestStatus.PENDING);
    }

    @Override
    @Transactional
    public DealEditRequest permit(Long requestId, DealEditRequestDtos.DecisionRequest request) {
        DealEditRequest entity = dealEditRequestRepository.findById(requestId)
                .orElseThrow(() -> new ResourceNotFoundException("Deal edit request not found"));

        if (entity.getStatus() != DealEditRequestStatus.PENDING) {
            throw new BadRequestException("Only PENDING requests can be permitted");
        }

        User currentUser = getCurrentUser();

        DealDtos.UpdateRequest changes = deserializeChanges(entity.getRequestedChanges());

        // Apply changes to deal using existing update flow
        Deal updatedDeal = dealService.update(entity.getDeal().getId(), changes);

        entity.setStatus(DealEditRequestStatus.PERMITTED);
        entity.setProcessedBy(currentUser);
        entity.setProcessedAt(LocalDateTime.now());
        if (request != null) {
            entity.setAdminComment(request.adminComment);
        }

        dealEditRequestRepository.save(entity);

        // Update attached deal reference to latest state (optional)
        entity.setDeal(updatedDeal);
        return entity;
    }

    @Override
    @Transactional
    public DealEditRequest reject(Long requestId, DealEditRequestDtos.DecisionRequest request) {
        DealEditRequest entity = dealEditRequestRepository.findById(requestId)
                .orElseThrow(() -> new ResourceNotFoundException("Deal edit request not found"));

        if (entity.getStatus() != DealEditRequestStatus.PENDING) {
            throw new BadRequestException("Only PENDING requests can be rejected");
        }

        User currentUser = getCurrentUser();
        entity.setStatus(DealEditRequestStatus.REJECTED);
        entity.setProcessedBy(currentUser);
        entity.setProcessedAt(LocalDateTime.now());
        if (request != null) {
            entity.setAdminComment(request.adminComment);
        }

        return dealEditRequestRepository.save(entity);
    }

    private DealDtos.UpdateRequest deserializeChanges(String json) {
        try {
            return objectMapper.readValue(json, DealDtos.UpdateRequest.class);
        } catch (Exception e) {
            throw new BadRequestException("Stored changes payload is invalid");
        }
    }

    private User getCurrentUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !(authentication.getPrincipal() instanceof UserDetails)) {
            throw new UnauthorizedException("User not authenticated");
        }
        String email = ((UserDetails) authentication.getPrincipal()).getUsername();
        return userRepository.findByEmail(email)
                .orElseThrow(() -> new UnauthorizedException("Current user not found"));
    }
}

