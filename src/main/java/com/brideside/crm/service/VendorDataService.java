package com.brideside.crm.service;

import com.brideside.crm.dto.VendorDataDtos;

public interface VendorDataService {

    VendorDataDtos.VendorDataResponse getByVendor(Long organizationId, Long vendorId);

    VendorDataDtos.VendorDataResponse create(Long organizationId, Long vendorId, VendorDataDtos.VendorDataCreateRequest request);

    VendorDataDtos.VendorDataResponse update(Long organizationId, Long vendorId, Long dataId, VendorDataDtos.VendorDataUpdateRequest request);

    VendorDataDtos.VendorDataResponse save(Long organizationId, Long vendorId, VendorDataDtos.VendorDataUpdateRequest request);

    void delete(Long organizationId, Long vendorId);

    void deleteById(Long organizationId, Long vendorId, Long dataId);
}
