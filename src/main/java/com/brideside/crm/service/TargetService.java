package com.brideside.crm.service;

import com.brideside.crm.dto.TargetDtos;

import java.util.List;

public interface TargetService {

    TargetDtos.TargetResponse get(Long id);

    TargetDtos.TargetResponse create(TargetDtos.TargetUpsertRequest request);

    TargetDtos.TargetResponse update(Long id, TargetDtos.TargetUpsertRequest request);

    void delete(Long id);

    List<TargetDtos.TargetResponse> list(TargetDtos.TargetListFilter filter);

    TargetDtos.DashboardResponse dashboard(TargetDtos.DashboardFilter filter);

    TargetDtos.CategoryMonthlyBreakdownResponse categoryMonthlyBreakdown(TargetDtos.DashboardFilter filter);

    TargetDtos.FiltersResponse filters();

    // New methods for sales users and organizations
    List<TargetDtos.SalesUserWithOrganizations> getSalesUsersWithOrganizations();

    TargetDtos.SalesUserOrganizationsResponse getSalesUserOrganizations(Long userId);
    
    TargetDtos.TargetUserMonthlyDetailResponse getUserMonthlyDetail(Long userId, int year);
}

