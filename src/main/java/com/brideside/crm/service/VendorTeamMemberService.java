package com.brideside.crm.service;

import com.brideside.crm.dto.VendorTeamMemberDtos;

import java.util.List;

public interface VendorTeamMemberService {

    List<VendorTeamMemberDtos.TeamMemberResponse> listByVendor(Long organizationId, Long vendorId);

    VendorTeamMemberDtos.TeamMemberResponse create(Long organizationId, Long vendorId, VendorTeamMemberDtos.TeamMemberCreateRequest request);

    VendorTeamMemberDtos.TeamMemberResponse update(Long organizationId, Long vendorId, Long memberId, VendorTeamMemberDtos.TeamMemberUpdateRequest request);

    void delete(Long organizationId, Long vendorId, Long memberId);
}
