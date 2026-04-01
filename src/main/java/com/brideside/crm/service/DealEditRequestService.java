package com.brideside.crm.service;

import com.brideside.crm.dto.DealEditRequestDtos;
import com.brideside.crm.entity.DealEditRequest;

import java.util.List;

public interface DealEditRequestService {

    DealEditRequest create(Long dealId, DealEditRequestDtos.CreateRequest request);

    List<DealEditRequest> listPending();

    DealEditRequest permit(Long requestId, DealEditRequestDtos.DecisionRequest request);

    DealEditRequest reject(Long requestId, DealEditRequestDtos.DecisionRequest request);
}

