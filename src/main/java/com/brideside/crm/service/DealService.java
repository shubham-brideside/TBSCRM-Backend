package com.brideside.crm.service;

import com.brideside.crm.dto.DealDtos;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.DealStatus;

import java.util.List;

public interface DealService {
    Deal create(DealDtos.CreateRequest request);
    Deal get(Long id);
    List<Deal> list();
    List<Deal> listWon();
    List<Deal> listByStatus(DealStatus status);
    List<Deal> listByPerson(Long personId);
    List<Deal> listByOrganization(Long organizationId);
    List<Deal> listByCategory(Long categoryId);
    Deal updateStage(Long id, DealDtos.UpdateStageRequest request);
    Deal markStatus(Long id, DealStatus status);
}



