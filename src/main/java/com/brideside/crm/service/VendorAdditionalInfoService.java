package com.brideside.crm.service;

import com.brideside.crm.dto.VendorAdditionalInfoDtos;
import org.springframework.web.multipart.MultipartFile;

public interface VendorAdditionalInfoService {

    VendorAdditionalInfoDtos.AdditionalInfoResponse getByVendor(Long organizationId, Long vendorId);

    VendorAdditionalInfoDtos.AdditionalInfoResponse saveForVendor(Long organizationId, Long vendorId,
                                                                  VendorAdditionalInfoDtos.AdditionalInfoSaveRequest request);

    VendorAdditionalInfoDtos.AdditionalInfoResponse uploadVendorContract(Long organizationId, Long vendorId,
                                                                         MultipartFile file);

    VendorAdditionalInfoDtos.AdditionalInfoResponse uploadVendorLogo(Long organizationId, Long vendorId,
                                                                     MultipartFile file);
}

