package com.brideside.crm.service;

import com.brideside.crm.dto.BridesideVendorDtos;

import java.util.List;

public interface BridesideVendorService {

    List<BridesideVendorDtos.VendorResponse> listByOrganizationId(Long organizationId);

    BridesideVendorDtos.VendorResponse createVendorForOrganization(Long organizationId, BridesideVendorDtos.VendorCreateRequest request);

    BridesideVendorDtos.VendorResponse updateVendorDetails(Long organizationId, Long vendorId, BridesideVendorDtos.VendorUpdateRequest request);
}

