package com.brideside.crm.service;

import com.brideside.crm.dto.VendorTeamSizeDtos;

public interface VendorTeamSizeService {

    VendorTeamSizeDtos.TeamSizeRowsResponse listByVendor(Long organizationId, Long vendorId);

    VendorTeamSizeDtos.TeamSizeRowsResponse saveForVendor(Long organizationId, Long vendorId, VendorTeamSizeDtos.TeamSizeSaveRequest request);
}

