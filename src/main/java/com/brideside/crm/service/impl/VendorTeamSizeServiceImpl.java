package com.brideside.crm.service.impl;

import com.brideside.crm.dto.VendorTeamSizeDtos;
import com.brideside.crm.entity.BridesideVendor;
import com.brideside.crm.entity.VendorTeamSize;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.BridesideVendorRepository;
import com.brideside.crm.repository.VendorTeamSizeRepository;
import com.brideside.crm.service.OrganizationProgressService;
import com.brideside.crm.service.VendorTeamSizeService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@Transactional
public class VendorTeamSizeServiceImpl implements VendorTeamSizeService {

    private final VendorTeamSizeRepository vendorTeamSizeRepository;
    private final BridesideVendorRepository bridesideVendorRepository;
    private final OrganizationProgressService organizationProgressService;

    public VendorTeamSizeServiceImpl(VendorTeamSizeRepository vendorTeamSizeRepository,
                                     BridesideVendorRepository bridesideVendorRepository,
                                     OrganizationProgressService organizationProgressService) {
        this.vendorTeamSizeRepository = vendorTeamSizeRepository;
        this.bridesideVendorRepository = bridesideVendorRepository;
        this.organizationProgressService = organizationProgressService;
    }

    @Override
    @Transactional(readOnly = true)
    public VendorTeamSizeDtos.TeamSizeRowsResponse listByVendor(Long organizationId, Long vendorId) {
        validateOrgAndVendor(organizationId, vendorId);
        List<VendorTeamSize> rows = vendorTeamSizeRepository.findByVendor_IdOrderByIdAsc(vendorId);
        VendorTeamSizeDtos.TeamSizeRowsResponse response = new VendorTeamSizeDtos.TeamSizeRowsResponse();
        response.setRows(rows.stream().map(this::toResponse).collect(Collectors.toList()));
        return response;
    }

    @Override
    public VendorTeamSizeDtos.TeamSizeRowsResponse saveForVendor(Long organizationId, Long vendorId, VendorTeamSizeDtos.TeamSizeSaveRequest request) {
        BridesideVendor vendor = validateOrgAndVendor(organizationId, vendorId);
        List<VendorTeamSizeDtos.TeamSizeRowRequest> rowRequests =
                request != null && request.getRows() != null ? request.getRows() : new ArrayList<>();

        if (rowRequests.isEmpty()) {
            vendorTeamSizeRepository.deleteByVendor_Id(vendorId);
        } else {
            List<VendorTeamSize> existing = vendorTeamSizeRepository.findByVendor_IdOrderByIdAsc(vendorId);
            List<VendorTeamSize> toSave = new ArrayList<>();

            for (VendorTeamSizeDtos.TeamSizeRowRequest rowRequest : rowRequests) {
                VendorTeamSize row = null;
                if (rowRequest.getId() != null) {
                    Long id = rowRequest.getId();
                    Optional<VendorTeamSize> maybeExisting = existing.stream()
                            .filter(r -> Objects.equals(r.getId(), id))
                            .findFirst();
                    if (maybeExisting.isPresent()) {
                        row = maybeExisting.get();
                    }
                }
                if (row == null) {
                    row = new VendorTeamSize();
                    row.setVendor(vendor);
                }
                row.setGuestCount(trimmed(rowRequest.getGuestCount()));
                row.setEventType(trimmed(rowRequest.getEventType()));
                row.setPhotographer(trimmedAllowEmpty(rowRequest.getPhotographer()));
                row.setCinematographer(trimmedAllowEmpty(rowRequest.getCinematographer()));
                row.setDrone(trimmedAllowEmpty(rowRequest.getDrone()));
                row.setNotes(trimmedAllowEmpty(rowRequest.getNotes()));
                toSave.add(row);
            }

            vendorTeamSizeRepository.deleteByVendor_Id(vendorId);
            vendorTeamSizeRepository.saveAll(toSave);
        }

        organizationProgressService.recomputeAndPersistProgress(organizationId);
        List<VendorTeamSize> savedRows = vendorTeamSizeRepository.findByVendor_IdOrderByIdAsc(vendorId);
        VendorTeamSizeDtos.TeamSizeRowsResponse response = new VendorTeamSizeDtos.TeamSizeRowsResponse();
        response.setRows(savedRows.stream().map(this::toResponse).collect(Collectors.toList()));
        return response;
    }

    private BridesideVendor validateOrgAndVendor(Long organizationId, Long vendorId) {
        if (organizationId == null || vendorId == null) {
            throw new BadRequestException("Organization id and vendor id are required");
        }
        return bridesideVendorRepository.findByIdAndOrganization_Id(vendorId, organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Vendor not found with id " + vendorId + " for organization " + organizationId));
    }

    private VendorTeamSizeDtos.TeamSizeRowResponse toResponse(VendorTeamSize entity) {
        VendorTeamSizeDtos.TeamSizeRowResponse response = new VendorTeamSizeDtos.TeamSizeRowResponse();
        response.setId(entity.getId());
        response.setVendorId(entity.getVendor() != null ? entity.getVendor().getId() : null);
        response.setGuestCount(entity.getGuestCount());
        response.setEventType(entity.getEventType());
        response.setPhotographer(entity.getPhotographer());
        response.setCinematographer(entity.getCinematographer());
        response.setDrone(entity.getDrone());
        response.setNotes(entity.getNotes());
        response.setCreatedAt(entity.getCreatedAt());
        response.setUpdatedAt(entity.getUpdatedAt());
        return response;
    }

    private String trimmed(String value) {
        return value == null ? null : value.trim();
    }

    private String trimmedAllowEmpty(String value) {
        if (value == null) {
            return null;
        }
        return value.trim();
    }
}

