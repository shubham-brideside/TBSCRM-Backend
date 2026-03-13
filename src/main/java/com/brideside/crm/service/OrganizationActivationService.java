package com.brideside.crm.service;

import com.brideside.crm.dto.OrganizationActivationDtos;
import com.fasterxml.jackson.databind.JsonNode;

public interface OrganizationActivationService {

    OrganizationActivationDtos.ProgressResponse getActivationProgress(Long organizationId);

    OrganizationActivationDtos.ProgressResponse saveChecklist(
            Long organizationId,
            OrganizationActivationDtos.Checklist checklist,
            JsonNode rawBody);
}

