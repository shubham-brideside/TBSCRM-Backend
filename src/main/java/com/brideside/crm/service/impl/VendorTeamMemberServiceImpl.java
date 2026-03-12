package com.brideside.crm.service.impl;

import com.brideside.crm.dto.VendorTeamMemberDtos;
import com.brideside.crm.entity.BridesideVendor;
import com.brideside.crm.entity.VendorTeamMember;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.BridesideVendorRepository;
import com.brideside.crm.repository.VendorTeamMemberRepository;
import com.brideside.crm.service.OrganizationProgressService;
import com.brideside.crm.service.VendorTeamMemberService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Service
@Transactional
public class VendorTeamMemberServiceImpl implements VendorTeamMemberService {

    private final VendorTeamMemberRepository vendorTeamMemberRepository;
    private final BridesideVendorRepository bridesideVendorRepository;
    private final OrganizationProgressService organizationProgressService;

    public VendorTeamMemberServiceImpl(VendorTeamMemberRepository vendorTeamMemberRepository,
                                       BridesideVendorRepository bridesideVendorRepository,
                                       OrganizationProgressService organizationProgressService) {
        this.vendorTeamMemberRepository = vendorTeamMemberRepository;
        this.bridesideVendorRepository = bridesideVendorRepository;
        this.organizationProgressService = organizationProgressService;
    }

    @Override
    @Transactional(readOnly = true)
    public List<VendorTeamMemberDtos.TeamMemberResponse> listByVendor(Long organizationId, Long vendorId) {
        validateOrgAndVendor(organizationId, vendorId);
        return vendorTeamMemberRepository.findByVendor_IdOrderByIdAsc(vendorId)
                .stream()
                .map(this::toResponse)
                .collect(Collectors.toList());
    }

    @Override
    public VendorTeamMemberDtos.TeamMemberResponse create(Long organizationId, Long vendorId, VendorTeamMemberDtos.TeamMemberCreateRequest request) {
        BridesideVendor vendor = validateOrgAndVendor(organizationId, vendorId);
        VendorTeamMember member = new VendorTeamMember();
        member.setVendor(vendor);
        member.setName(trimmed(request.getName()));
        member.setDesignation(trimmed(request.getDesignation()));
        member.setInstagramId(trimmed(request.getInstagramId()));
        VendorTeamMember saved = vendorTeamMemberRepository.save(member);
        organizationProgressService.recomputeAndPersistProgress(organizationId);
        return toResponse(saved);
    }

    @Override
    public VendorTeamMemberDtos.TeamMemberResponse update(Long organizationId, Long vendorId, Long memberId, VendorTeamMemberDtos.TeamMemberUpdateRequest request) {
        validateOrgAndVendor(organizationId, vendorId);
        VendorTeamMember member = vendorTeamMemberRepository.findByIdAndVendor_Id(memberId, vendorId)
                .orElseThrow(() -> new ResourceNotFoundException("Team member not found with id " + memberId));
        member.setName(trimmed(request.getName()));
        member.setDesignation(trimmed(request.getDesignation()));
        member.setInstagramId(trimmed(request.getInstagramId()));
        VendorTeamMember saved = vendorTeamMemberRepository.save(member);
        organizationProgressService.recomputeAndPersistProgress(organizationId);
        return toResponse(saved);
    }

    @Override
    public void delete(Long organizationId, Long vendorId, Long memberId) {
        validateOrgAndVendor(organizationId, vendorId);
        VendorTeamMember member = vendorTeamMemberRepository.findByIdAndVendor_Id(memberId, vendorId)
                .orElseThrow(() -> new ResourceNotFoundException("Team member not found with id " + memberId));
        vendorTeamMemberRepository.delete(member);
        organizationProgressService.recomputeAndPersistProgress(organizationId);
    }

    private BridesideVendor validateOrgAndVendor(Long organizationId, Long vendorId) {
        if (organizationId == null || vendorId == null) {
            throw new BadRequestException("Organization id and vendor id are required");
        }
        return bridesideVendorRepository.findByIdAndOrganization_Id(vendorId, organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Vendor not found with id " + vendorId + " for organization " + organizationId));
    }

    private VendorTeamMemberDtos.TeamMemberResponse toResponse(VendorTeamMember member) {
        VendorTeamMemberDtos.TeamMemberResponse response = new VendorTeamMemberDtos.TeamMemberResponse();
        response.setId(member.getId());
        response.setVendorId(member.getVendor() != null ? member.getVendor().getId() : null);
        response.setName(member.getName());
        response.setDesignation(member.getDesignation());
        response.setInstagramId(member.getInstagramId());
        response.setCreatedAt(member.getCreatedAt());
        response.setUpdatedAt(member.getUpdatedAt());
        return response;
    }

    private String trimmed(String value) {
        return value == null ? null : value.trim();
    }
}
