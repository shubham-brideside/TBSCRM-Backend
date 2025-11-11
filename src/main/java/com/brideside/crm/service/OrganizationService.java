package com.brideside.crm.service;

import com.brideside.crm.dto.OrganizationDtos;

import java.util.List;

public interface OrganizationService {

    OrganizationDtos.OrganizationResponse create(OrganizationDtos.OrganizationRequest request);

    List<OrganizationDtos.OrganizationResponse> list();

    OrganizationDtos.OrganizationResponse get(Long id);

    OrganizationDtos.OrganizationResponse update(Long id, OrganizationDtos.OrganizationRequest request);

    void delete(Long id);

    List<OrganizationDtos.OwnerOption> listOwnerOptions();

    List<OrganizationDtos.CategoryOption> listCategoryOptions();
}


