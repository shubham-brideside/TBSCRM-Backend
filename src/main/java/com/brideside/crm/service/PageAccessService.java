package com.brideside.crm.service;

import com.brideside.crm.dto.PageAccessDtos;

import java.util.List;

public interface PageAccessService {
    PageAccessDtos.PageAccessResponse getUserPageAccess(Long userId, String currentUserEmail);
    PageAccessDtos.PageAccessResponse getCurrentUserPageAccess(String currentUserEmail);
    PageAccessDtos.PageAccessResponse updateUserPageAccess(Long userId, PageAccessDtos.UpdatePageAccessRequest request, String currentUserEmail);
    PageAccessDtos.SinglePageAccessResponse updateSinglePageAccess(Long userId, String pageName, PageAccessDtos.UpdateSinglePageAccessRequest request, String currentUserEmail);
    List<PageAccessDtos.PageAccessSummaryResponse> getAllUsersPageAccessSummary(String currentUserEmail);
}

