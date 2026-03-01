package com.brideside.crm.service.impl;

import com.brideside.crm.dto.BridesideVendorDtos;
import com.brideside.crm.dto.PipelineDtos;
import com.brideside.crm.entity.BridesideVendor;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Pipeline;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.BridesideVendorRepository;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.repository.PipelineRepository;
import com.brideside.crm.service.PipelineService;
import com.brideside.crm.service.BridesideVendorService;
import com.brideside.crm.service.VendorAssetService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Service
@Transactional
public class BridesideVendorServiceImpl implements BridesideVendorService {

    private final BridesideVendorRepository bridesideVendorRepository;
    private final OrganizationRepository organizationRepository;
    private final PipelineRepository pipelineRepository;
    private final PipelineService pipelineService;
    private final VendorAssetService vendorAssetService;

    public BridesideVendorServiceImpl(BridesideVendorRepository bridesideVendorRepository,
                                     OrganizationRepository organizationRepository,
                                     PipelineRepository pipelineRepository,
                                     PipelineService pipelineService,
                                     VendorAssetService vendorAssetService) {
        this.bridesideVendorRepository = bridesideVendorRepository;
        this.organizationRepository = organizationRepository;
        this.pipelineRepository = pipelineRepository;
        this.pipelineService = pipelineService;
        this.vendorAssetService = vendorAssetService;
    }

    @Override
    @Transactional(readOnly = true)
    public List<BridesideVendorDtos.VendorResponse> listByOrganizationId(Long organizationId) {
        if (organizationId == null) {
            throw new ResourceNotFoundException("Organization id is required");
        }
        if (!organizationRepository.existsById(organizationId)) {
            throw new ResourceNotFoundException("Organization not found with id " + organizationId);
        }
        return bridesideVendorRepository.findByOrganization_Id(organizationId)
                .stream()
                .map(this::toResponse)
                .collect(Collectors.toList());
    }

    @Override
    public BridesideVendorDtos.VendorResponse createVendorForOrganization(Long organizationId, BridesideVendorDtos.VendorCreateRequest request) {
        if (organizationId == null) {
            throw new BadRequestException("Organization id is required");
        }
        Organization organization = organizationRepository.findById(organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Organization not found with id " + organizationId));

        List<BridesideVendor> existing = bridesideVendorRepository.findByOrganization_Id(organizationId);
        if (!existing.isEmpty()) {
            return toResponse(existing.get(0));
        }

        BridesideVendorDtos.VendorCreateRequest safeRequest = request == null ? new BridesideVendorDtos.VendorCreateRequest() : request;

        Pipeline pipeline = resolveOrCreatePipeline(organization, safeRequest.getPipelineId());
        String username = resolveOrGenerateUsername(organizationId, safeRequest.getUsername());

        BridesideVendor vendor = new BridesideVendor();
        vendor.setOrganization(organization);
        vendor.setPipeline(pipeline);
        vendor.setUsername(username);

        vendor.setIgAccountId(trimmed(safeRequest.getIgAccountId()));
        vendor.setBusinessName(trimmed(safeRequest.getBusinessName()));
        vendor.setVendorName(trimmed(safeRequest.getVendorName()));
        vendor.setServices(trimmed(safeRequest.getServices()));

        // Use org email and address when creating vendor (from organization creation)
        String orgEmail = trimmed(organization.getEmail());
        String orgAddress = trimmed(organization.getAddress());
        vendor.setEmailId(orgEmail != null && !orgEmail.isBlank() ? orgEmail : trimmed(safeRequest.getEmailId()));
        vendor.setOfficeStudioLocation(orgAddress != null && !orgAddress.isBlank() ? orgAddress : trimmed(safeRequest.getOfficeStudioLocation()));
        vendor.setContactNumber(trimmed(safeRequest.getContactNumber()));
        vendor.setBaseLocation(trimmed(safeRequest.getBaseLocation()));
        vendor.setOfficialNumber(trimmed(safeRequest.getOfficialNumber()));
        vendor.setOnboardingDate(safeRequest.getOnboardingDate());
        vendor.setTeamSize(safeRequest.getTeamSize());
        vendor.setOnboardingFee(safeRequest.getOnboardingFee());

        BridesideVendor saved = bridesideVendorRepository.save(vendor);
        vendorAssetService.createForVendor(saved.getId(), organization.getId());
        return toResponse(saved);
    }

    @Override
    public BridesideVendorDtos.VendorResponse updateVendorDetails(Long organizationId, Long vendorId, BridesideVendorDtos.VendorUpdateRequest request) {
        if (organizationId == null) {
            throw new BadRequestException("Organization id is required");
        }
        if (vendorId == null) {
            throw new BadRequestException("Vendor id is required");
        }
        BridesideVendor vendor = bridesideVendorRepository.findByIdAndOrganization_Id(vendorId, organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Vendor not found with id " + vendorId + " for organization " + organizationId));

        if (request.getPipelineId() != null) {
            Pipeline pipeline = pipelineRepository.findById(request.getPipelineId())
                    .orElseThrow(() -> new ResourceNotFoundException("Pipeline not found with id " + request.getPipelineId()));
            Long pipelineOrgId = pipeline.getOrganization() != null ? pipeline.getOrganization().getId() : null;
            if (pipelineOrgId == null || !pipelineOrgId.equals(organizationId)) {
                throw new BadRequestException("Pipeline " + request.getPipelineId() + " does not belong to organization " + organizationId);
            }
            vendor.setPipeline(pipeline);
        }

        vendor.setVendorName(trimmed(request.getVendorName()));
        vendor.setContactNumber(trimmed(request.getContactNumber()));
        vendor.setOfficeStudioLocation(trimmed(request.getOfficeStudioLocation()));
        vendor.setBaseLocation(trimmed(request.getBaseLocation()));
        vendor.setOfficialNumber(trimmed(request.getOfficialNumber()));
        vendor.setEmailId(trimmed(request.getEmailId()));
        vendor.setOnboardingDate(request.getOnboardingDate());
        vendor.setTeamSize(request.getTeamSize());
        vendor.setOnboardingFee(request.getOnboardingFee());

        return toResponse(bridesideVendorRepository.save(vendor));
    }

    private Pipeline resolveOrCreatePipeline(Organization organization, Long pipelineId) {
        if (pipelineId != null) {
            Pipeline pipeline = pipelineRepository.findById(pipelineId)
                    .orElseThrow(() -> new ResourceNotFoundException("Pipeline not found with id " + pipelineId));
            Long pipelineOrgId = pipeline.getOrganization() != null ? pipeline.getOrganization().getId() : null;
            if (pipelineOrgId == null || !pipelineOrgId.equals(organization.getId())) {
                throw new BadRequestException("Pipeline " + pipelineId + " does not belong to organization " + organization.getId());
            }
            return pipeline;
        }

        List<Pipeline> pipelines = pipelineRepository.findByOrganization(organization);
        for (Pipeline p : pipelines) {
            if (p != null && Boolean.FALSE.equals(p.getDeleted())) {
                return p;
            }
        }

        PipelineDtos.PipelineRequest createRequest = new PipelineDtos.PipelineRequest();
        createRequest.setName("Default Pipeline - Org " + organization.getId());
        createRequest.setOrganizationId(organization.getId());
        PipelineDtos.PipelineResponse created = pipelineService.createPipeline(createRequest);
        return pipelineRepository.findById(created.getId())
                .orElseThrow(() -> new ResourceNotFoundException("Pipeline not found with id " + created.getId()));
    }

    private String resolveOrGenerateUsername(Long organizationId, String requestedUsername) {
        String base = trimmed(requestedUsername);
        if (base == null || base.isBlank()) {
            base = "vendor-org-" + organizationId;
        }

        String candidate = base;
        int i = 1;
        while (bridesideVendorRepository.existsByUsername(candidate)) {
            candidate = base + "-" + i;
            i++;
            if (i > 1000) {
                throw new BadRequestException("Unable to generate a unique username for vendor");
            }
        }
        return candidate;
    }

    private BridesideVendorDtos.VendorResponse toResponse(BridesideVendor vendor) {
        BridesideVendorDtos.VendorResponse response = new BridesideVendorDtos.VendorResponse();
        response.setId(vendor.getId());
        response.setUsername(vendor.getUsername());
        response.setOrganizationId(vendor.getOrganization() != null ? vendor.getOrganization().getId() : null);
        response.setPipelineId(vendor.getPipeline() != null ? vendor.getPipeline().getId() : null);
        response.setIgAccountId(vendor.getIgAccountId());
        response.setBusinessName(vendor.getBusinessName());
        response.setVendorName(vendor.getVendorName());
        response.setServices(vendor.getServices());
        response.setAccountOwner(toAccountOwnerSummary(vendor.getAccountOwner()));

        response.setContactNumber(vendor.getContactNumber());
        response.setOfficeStudioLocation(vendor.getOfficeStudioLocation());
        response.setBaseLocation(vendor.getBaseLocation());
        response.setOfficialNumber(vendor.getOfficialNumber());
        response.setEmailId(vendor.getEmailId());
        response.setOnboardingDate(vendor.getOnboardingDate());
        response.setTeamSize(vendor.getTeamSize());
        response.setOnboardingFee(vendor.getOnboardingFee());

        response.setCreatedAt(vendor.getCreatedAt());
        response.setUpdatedAt(vendor.getUpdatedAt());
        return response;
    }

    private BridesideVendorDtos.AccountOwnerSummary toAccountOwnerSummary(User user) {
        if (user == null) {
            return null;
        }
        BridesideVendorDtos.AccountOwnerSummary summary = new BridesideVendorDtos.AccountOwnerSummary();
        summary.setId(user.getId());
        summary.setFirstName(user.getFirstName());
        summary.setLastName(user.getLastName());
        summary.setEmail(user.getEmail());
        return summary;
    }

    private String trimmed(String value) {
        return value == null ? null : value.trim();
    }
}

