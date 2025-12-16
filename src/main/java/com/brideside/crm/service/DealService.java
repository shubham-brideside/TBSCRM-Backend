package com.brideside.crm.service;

import com.brideside.crm.dto.DealDtos;
import com.brideside.crm.dto.PipelineDtos;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.DealStatus;

import java.util.List;

public interface DealService {
    Deal create(DealDtos.CreateRequest request);
    Deal get(Long id);
    List<Deal> list();
    List<Deal> list(String sortField, String sortDirection);
    List<Deal> list(Long pipelineId, String status, Long organizationId, Long categoryId, 
                    Long managerId, String dateFrom, String dateTo, String search,
                    String sortField, String sortDirection);
    List<Deal> listWon();
    List<Deal> listByStatus(DealStatus status);
    List<Deal> listByPerson(Long personId);
    List<Deal> listByOrganization(Long organizationId);
    List<Deal> listByCategory(Long categoryId);
    Deal update(Long id, DealDtos.UpdateRequest request);
    Deal updateStage(Long id, DealDtos.UpdateStageRequest request);
    Deal markStatus(Long id, DealDtos.MarkStatusRequest request);
    void delete(Long id);
    List<PipelineDtos.PipelineResponse> getAvailablePipelinesForDiversion(Long dealId);
}



