package com.brideside.crm.service.impl;

import com.brideside.crm.dto.VendorDataDtos;
import com.brideside.crm.entity.BridesideVendor;
import com.brideside.crm.entity.VendorData;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.BridesideVendorRepository;
import com.brideside.crm.repository.VendorDataRepository;
import com.brideside.crm.service.VendorDataService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class VendorDataServiceImpl implements VendorDataService {

    private final VendorDataRepository vendorDataRepository;
    private final BridesideVendorRepository bridesideVendorRepository;

    public VendorDataServiceImpl(VendorDataRepository vendorDataRepository,
                                 BridesideVendorRepository bridesideVendorRepository) {
        this.vendorDataRepository = vendorDataRepository;
        this.bridesideVendorRepository = bridesideVendorRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public VendorDataDtos.VendorDataResponse getByVendor(Long organizationId, Long vendorId) {
        BridesideVendor vendor = validateOrgAndVendor(organizationId, vendorId);
        return vendorDataRepository.findByVendor_Id(vendorId)
                .map(this::toResponse)
                .orElse(null);
    }

    @Override
    public VendorDataDtos.VendorDataResponse create(Long organizationId, Long vendorId, VendorDataDtos.VendorDataCreateRequest request) {
        BridesideVendor vendor = validateOrgAndVendor(organizationId, vendorId);
        if (vendorDataRepository.existsByVendor_Id(vendorId)) {
            throw new BadRequestException("Vendor data already exists for this vendor. Use update instead.");
        }
        VendorData data = new VendorData();
        data.setVendor(vendor);
        data.setMasterDataLink(trimmed(request.getMasterDataLink()));
        data.setCalendarSheetLink(trimmed(request.getCalendarSheetLink()));
        return toResponse(vendorDataRepository.save(data));
    }

    @Override
    public VendorDataDtos.VendorDataResponse update(Long organizationId, Long vendorId, Long dataId, VendorDataDtos.VendorDataUpdateRequest request) {
        validateOrgAndVendor(organizationId, vendorId);
        VendorData data = vendorDataRepository.findById(dataId)
                .orElseThrow(() -> new ResourceNotFoundException("Vendor data not found with id " + dataId));
        if (data.getVendor() == null || !data.getVendor().getId().equals(vendorId)) {
            throw new ResourceNotFoundException("Vendor data not found for vendor " + vendorId);
        }
        data.setMasterDataLink(trimmed(request.getMasterDataLink()));
        data.setCalendarSheetLink(trimmed(request.getCalendarSheetLink()));
        return toResponse(vendorDataRepository.save(data));
    }

    @Override
    public VendorDataDtos.VendorDataResponse save(Long organizationId, Long vendorId, VendorDataDtos.VendorDataUpdateRequest request) {
        BridesideVendor vendor = validateOrgAndVendor(organizationId, vendorId);
        VendorData data = vendorDataRepository.findByVendor_Id(vendorId)
                .orElseGet(() -> {
                    VendorData newData = new VendorData();
                    newData.setVendor(vendor);
                    return newData;
                });
        data.setMasterDataLink(trimmed(request.getMasterDataLink()));
        data.setCalendarSheetLink(trimmed(request.getCalendarSheetLink()));
        return toResponse(vendorDataRepository.save(data));
    }

    @Override
    public void delete(Long organizationId, Long vendorId) {
        validateOrgAndVendor(organizationId, vendorId);
        vendorDataRepository.findByVendor_Id(vendorId)
                .ifPresent(vendorDataRepository::delete);
    }

    @Override
    public void deleteById(Long organizationId, Long vendorId, Long dataId) {
        validateOrgAndVendor(organizationId, vendorId);
        VendorData data = vendorDataRepository.findById(dataId)
                .orElseThrow(() -> new ResourceNotFoundException("Vendor data not found with id " + dataId));
        if (data.getVendor() == null || !data.getVendor().getId().equals(vendorId)) {
            throw new ResourceNotFoundException("Vendor data not found for vendor " + vendorId);
        }
        vendorDataRepository.delete(data);
    }

    private BridesideVendor validateOrgAndVendor(Long organizationId, Long vendorId) {
        if (organizationId == null || vendorId == null) {
            throw new BadRequestException("Organization id and vendor id are required");
        }
        return bridesideVendorRepository.findByIdAndOrganization_Id(vendorId, organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Vendor not found with id " + vendorId + " for organization " + organizationId));
    }

    private VendorDataDtos.VendorDataResponse toResponse(VendorData data) {
        VendorDataDtos.VendorDataResponse response = new VendorDataDtos.VendorDataResponse();
        response.setId(data.getId());
        response.setVendorId(data.getVendor() != null ? data.getVendor().getId() : null);
        response.setMasterDataLink(data.getMasterDataLink());
        response.setCalendarSheetLink(data.getCalendarSheetLink());
        response.setCreatedAt(data.getCreatedAt());
        response.setUpdatedAt(data.getUpdatedAt());
        return response;
    }

    private String trimmed(String value) {
        return value == null ? null : value.trim();
    }
}
