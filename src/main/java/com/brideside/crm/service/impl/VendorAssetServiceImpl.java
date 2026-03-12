package com.brideside.crm.service.impl;

import com.brideside.crm.dto.VendorAssetDtos;
import com.brideside.crm.entity.BridesideVendor;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.VendorAsset;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.BridesideVendorRepository;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.repository.VendorAssetRepository;
import com.brideside.crm.service.OrganizationProgressService;
import com.brideside.crm.service.VendorAssetService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Service
@Transactional
public class VendorAssetServiceImpl implements VendorAssetService {

    private final VendorAssetRepository vendorAssetRepository;
    private final BridesideVendorRepository bridesideVendorRepository;
    private final OrganizationRepository organizationRepository;
    private final OrganizationProgressService organizationProgressService;

    public VendorAssetServiceImpl(VendorAssetRepository vendorAssetRepository,
                                  BridesideVendorRepository bridesideVendorRepository,
                                  OrganizationRepository organizationRepository,
                                  OrganizationProgressService organizationProgressService) {
        this.vendorAssetRepository = vendorAssetRepository;
        this.bridesideVendorRepository = bridesideVendorRepository;
        this.organizationRepository = organizationRepository;
        this.organizationProgressService = organizationProgressService;
    }

    @Override
    public VendorAssetDtos.AssetResponse createForVendor(Long vendorId, Long organizationId) {
        if (vendorId == null) {
            throw new BadRequestException("Vendor id is required");
        }
        if (organizationId == null) {
            throw new BadRequestException("Organization id is required");
        }
        BridesideVendor vendor = bridesideVendorRepository.findByIdAndOrganization_Id(vendorId, organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Vendor not found with id " + vendorId + " for organization " + organizationId));
        Organization organization = organizationRepository.findById(organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Organization not found with id " + organizationId));

        return vendorAssetRepository.findByVendor_IdAndOrganization_Id(vendorId, organizationId)
                .map(this::toResponse)
                .orElseGet(() -> {
                    VendorAsset asset = new VendorAsset();
                    asset.setVendor(vendor);
                    asset.setOrganization(organization);
                    return toResponse(vendorAssetRepository.save(asset));
                });
    }

    @Override
    public VendorAssetDtos.AssetResponse create(Long organizationId, Long vendorId, VendorAssetDtos.AssetUpdateRequest request) {
        if (organizationId == null || vendorId == null) {
            throw new BadRequestException("Organization id and vendor id are required");
        }
        BridesideVendor vendor = bridesideVendorRepository.findByIdAndOrganization_Id(vendorId, organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Vendor not found with id " + vendorId + " for organization " + organizationId));
        Organization organization = vendor.getOrganization();

        VendorAsset asset = new VendorAsset();
        asset.setVendor(vendor);
        asset.setOrganization(organization);
        if (request != null) {
            asset.setPhoneModel(trimmed(request.getPhoneModel()));
            asset.setPhoneIssuedBy(trimmed(request.getPhoneIssuedBy()));
            asset.setSimCard(trimmed(request.getSimCard()));
            asset.setSimIssuedBy(trimmed(request.getSimIssuedBy()));
            asset.setIssuedOn(request.getIssuedOn());
        }
        VendorAsset saved = vendorAssetRepository.save(asset);
        organizationProgressService.recomputeAndPersistProgress(organizationId);
        return toResponse(saved);
    }

    @Override
    @Transactional(readOnly = true)
    public List<VendorAssetDtos.AssetResponse> listByVendor(Long organizationId, Long vendorId) {
        if (organizationId == null || vendorId == null) {
            throw new BadRequestException("Organization id and vendor id are required");
        }
        if (!bridesideVendorRepository.findByIdAndOrganization_Id(vendorId, organizationId).isPresent()) {
            throw new ResourceNotFoundException("Vendor not found with id " + vendorId + " for organization " + organizationId);
        }
        return vendorAssetRepository.findByVendor_Id(vendorId)
                .stream()
                .map(this::toResponse)
                .collect(Collectors.toList());
    }

    @Override
    public VendorAssetDtos.AssetResponse update(Long organizationId, Long vendorId, Long assetId, VendorAssetDtos.AssetUpdateRequest request) {
        if (organizationId == null || vendorId == null || assetId == null) {
            throw new BadRequestException("Organization id, vendor id and asset id are required");
        }
        VendorAsset asset = vendorAssetRepository.findByIdAndVendor_Id(assetId, vendorId)
                .orElseThrow(() -> new ResourceNotFoundException("Vendor asset not found with id " + assetId));
        Long assetOrgId = asset.getOrganization() != null ? asset.getOrganization().getId() : null;
        if (assetOrgId == null || !assetOrgId.equals(organizationId)) {
            throw new ResourceNotFoundException("Vendor asset not found for organization " + organizationId);
        }

        asset.setPhoneModel(trimmed(request.getPhoneModel()));
        asset.setPhoneIssuedBy(trimmed(request.getPhoneIssuedBy()));
        asset.setSimCard(trimmed(request.getSimCard()));
        asset.setSimIssuedBy(trimmed(request.getSimIssuedBy()));
        asset.setIssuedOn(request.getIssuedOn());

        VendorAsset saved = vendorAssetRepository.save(asset);
        organizationProgressService.recomputeAndPersistProgress(organizationId);
        return toResponse(saved);
    }

    private VendorAssetDtos.AssetResponse toResponse(VendorAsset asset) {
        VendorAssetDtos.AssetResponse response = new VendorAssetDtos.AssetResponse();
        response.setId(asset.getId());
        response.setVendorId(asset.getVendor() != null ? asset.getVendor().getId() : null);
        response.setOrganizationId(asset.getOrganization() != null ? asset.getOrganization().getId() : null);
        response.setPhoneModel(asset.getPhoneModel());
        response.setPhoneIssuedBy(asset.getPhoneIssuedBy());
        response.setSimCard(asset.getSimCard());
        response.setSimIssuedBy(asset.getSimIssuedBy());
        response.setIssuedOn(asset.getIssuedOn());
        response.setCreatedAt(asset.getCreatedAt());
        response.setUpdatedAt(asset.getUpdatedAt());
        return response;
    }

    private String trimmed(String value) {
        return value == null ? null : value.trim();
    }
}
