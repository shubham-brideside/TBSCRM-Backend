package com.brideside.crm.service;

import com.brideside.crm.dto.VendorAssetDtos;

import java.util.List;

public interface VendorAssetService {

    VendorAssetDtos.AssetResponse createForVendor(Long vendorId, Long organizationId);

    List<VendorAssetDtos.AssetResponse> listByVendor(Long organizationId, Long vendorId);

    VendorAssetDtos.AssetResponse update(Long organizationId, Long vendorId, Long assetId, VendorAssetDtos.AssetUpdateRequest request);
}
