package com.brideside.crm.service.impl;

import com.brideside.crm.dto.OrganizationProgressDtos;
import com.brideside.crm.entity.BridesideVendor;
import com.brideside.crm.entity.ClientData;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.OrganizationOnboardingProgress;
import com.brideside.crm.entity.VendorAsset;
import com.brideside.crm.entity.VendorData;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.BridesideVendorRepository;
import com.brideside.crm.repository.ClientDataRepository;
import com.brideside.crm.repository.EventPricingRepository;
import com.brideside.crm.repository.OrganizationActivationRepository;
import com.brideside.crm.repository.OrganizationOnboardingProgressRepository;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.repository.VendorAssetRepository;
import com.brideside.crm.repository.VendorDataRepository;
import com.brideside.crm.repository.VendorTeamMemberRepository;
import com.brideside.crm.service.OrganizationProgressService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Optional;

@Service
public class OrganizationProgressServiceImpl implements OrganizationProgressService {

    private static final int TOTAL_SECTIONS = 6;

    private final OrganizationRepository organizationRepository;
    private final OrganizationOnboardingProgressRepository progressRepository;
    private final OrganizationActivationRepository organizationActivationRepository;
    private final BridesideVendorRepository bridesideVendorRepository;
    private final VendorAssetRepository vendorAssetRepository;
    private final EventPricingRepository eventPricingRepository;
    private final VendorDataRepository vendorDataRepository;
    private final ClientDataRepository clientDataRepository;
    private final VendorTeamMemberRepository vendorTeamMemberRepository;

    public OrganizationProgressServiceImpl(OrganizationRepository organizationRepository,
                                           OrganizationOnboardingProgressRepository progressRepository,
                                           OrganizationActivationRepository organizationActivationRepository,
                                           BridesideVendorRepository bridesideVendorRepository,
                                           VendorAssetRepository vendorAssetRepository,
                                           EventPricingRepository eventPricingRepository,
                                           VendorDataRepository vendorDataRepository,
                                           ClientDataRepository clientDataRepository,
                                           VendorTeamMemberRepository vendorTeamMemberRepository) {
        this.organizationRepository = organizationRepository;
        this.progressRepository = progressRepository;
        this.organizationActivationRepository = organizationActivationRepository;
        this.bridesideVendorRepository = bridesideVendorRepository;
        this.vendorAssetRepository = vendorAssetRepository;
        this.eventPricingRepository = eventPricingRepository;
        this.vendorDataRepository = vendorDataRepository;
        this.clientDataRepository = clientDataRepository;
        this.vendorTeamMemberRepository = vendorTeamMemberRepository;
    }

    @Override
    @Transactional
    public void recomputeAndPersistProgress(Long organizationId) {
        Organization organization = organizationRepository.findById(organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Organization not found with id " + organizationId));

        boolean orgDetails = isOrganizationDetailsComplete(organizationId);
        boolean assetInfo = isAssetInfoComplete(organizationId);
        boolean eventsPricing = isEventsPricingComplete(organizationId);
        boolean vendorData = isVendorDataComplete(organizationId);
        boolean clientData = isClientDataComplete(organizationId);
        boolean teamMembers = isTeamMembersComplete(organizationId);

        OrganizationOnboardingProgress progress = progressRepository.findByOrganization_Id(organizationId)
                .orElseGet(() -> {
                    OrganizationOnboardingProgress p = new OrganizationOnboardingProgress();
                    p.setOrganization(organization);
                    return p;
                });

        progress.setOrganizationDetailsComplete(orgDetails);
        progress.setAssetInfoComplete(assetInfo);
        progress.setEventsPricingComplete(eventsPricing);
        progress.setVendorDataComplete(vendorData);
        progress.setClientDataComplete(clientData);
        progress.setTeamMembersComplete(teamMembers);

        progressRepository.save(progress);

        boolean allComplete = orgDetails && assetInfo && eventsPricing && vendorData && clientData && teamMembers;
        boolean activationActivated = organizationActivationRepository.findByOrganization_Id(organizationId)
                .map(a -> Boolean.TRUE.equals(a.getActivated()))
                .orElse(false);
        // Progress API isActive: true if full onboarding done OR activation checklist activated (four core done)
        organization.setIsActive(allComplete || activationActivated);
        organizationRepository.save(organization);
    }

    @Override
    @Transactional
    public OrganizationProgressDtos.ProgressResponse getProgress(Long organizationId) {
        // Always recompute from DB so progress reflects current state (client data, vendor data, etc.)
        recomputeAndPersistProgress(organizationId);

        Organization organization = organizationRepository.findById(organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Organization not found with id " + organizationId));
        OrganizationOnboardingProgress progress = progressRepository.findByOrganization_Id(organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Progress not found for organization " + organizationId));

        boolean allOnboardingComplete = Boolean.TRUE.equals(progress.getOrganizationDetailsComplete())
                && Boolean.TRUE.equals(progress.getAssetInfoComplete())
                && Boolean.TRUE.equals(progress.getEventsPricingComplete())
                && Boolean.TRUE.equals(progress.getVendorDataComplete())
                && Boolean.TRUE.equals(progress.getClientDataComplete())
                && Boolean.TRUE.equals(progress.getTeamMembersComplete());
        boolean activationActivated = organizationActivationRepository.findByOrganization_Id(organizationId)
                .map(a -> Boolean.TRUE.equals(a.getActivated()))
                .orElse(false);
        // Progress isActive must match activation checklist when activated=true (same as organizations.is_active rule)
        boolean isActive = allOnboardingComplete || activationActivated;
        return toResponse(organization.getId(), progress, isActive);
    }

    @Override
    @Transactional
    public OrganizationProgressDtos.ProgressDebugResponse getProgressDebug(Long organizationId) {
        OrganizationProgressDtos.ProgressResponse base = getProgress(organizationId);
        OrganizationProgressDtos.ProgressDebugResponse debug = new OrganizationProgressDtos.ProgressDebugResponse();
        debug.setOrganizationId(base.getOrganizationId());
        debug.setOrganizationDetailsComplete(base.getOrganizationDetailsComplete());
        debug.setAssetInfoComplete(base.getAssetInfoComplete());
        debug.setEventsPricingComplete(base.getEventsPricingComplete());
        debug.setVendorDataComplete(base.getVendorDataComplete());
        debug.setClientDataComplete(base.getClientDataComplete());
        debug.setTeamMembersComplete(base.getTeamMembersComplete());
        debug.setIsActive(base.getIsActive());
        debug.setCompletedCount(base.getCompletedCount());
        debug.setTotalCount(base.getTotalCount());
        debug.setUpdatedAt(base.getUpdatedAt());

        Optional<ClientData> cd = clientDataRepository.findByOrganization_Id(organizationId);
        debug.setClientDataRecordExists(cd.isPresent());
        debug.setQuoteFormatUrlPresent(cd.map(c -> org.springframework.util.StringUtils.hasText(c.getQuoteFormatUrl())).orElse(false));
        debug.setClientContractFormatUrlPresent(cd.map(c -> org.springframework.util.StringUtils.hasText(c.getClientContractFormatUrl())).orElse(false));
        return debug;
    }

    /**
     * Organization details are stored in brideside_vendors. For each vendor of the organization,
     * we require: vendorName, contactNumber, emailId, (accountOwner or organization.owner), and at least one location
     * (officeStudioLocation or baseLocation).
     */
    private boolean isOrganizationDetailsComplete(Long organizationId) {
        List<BridesideVendor> vendors = bridesideVendorRepository.findByOrganization_Id(organizationId);
        if (vendors.isEmpty()) return false;
        Organization org = organizationRepository.findById(organizationId).orElse(null);
        if (org == null) return false;
        boolean orgHasOwner = org.getOwner() != null;
        for (BridesideVendor vendor : vendors) {
            boolean hasOwner = vendor.getAccountOwner() != null || orgHasOwner;
            if (!StringUtils.hasText(vendor.getVendorName())
                    || !StringUtils.hasText(vendor.getContactNumber())
                    || !StringUtils.hasText(vendor.getEmailId())
                    || !hasOwner
                    || (!StringUtils.hasText(vendor.getOfficeStudioLocation()) && !StringUtils.hasText(vendor.getBaseLocation()))) {
                return false;
            }
        }
        return true;
    }

    private boolean isAssetInfoComplete(Long organizationId) {
        List<BridesideVendor> vendors = bridesideVendorRepository.findByOrganization_Id(organizationId);
        if (vendors.isEmpty()) return false;
        for (BridesideVendor vendor : vendors) {
            List<VendorAsset> assets = vendorAssetRepository.findByVendor_Id(vendor.getId());
            if (assets.isEmpty()) return false;
            boolean hasCompleteAsset = assets.stream().anyMatch(this::hasMeaningfulAssetData);
            if (!hasCompleteAsset) return false;
        }
        return true;
    }

    /** Asset is considered complete only if at least one of phone_model, sim_card, issued_on, phone_issued_by, sim_issued_by is set. */
    private boolean hasMeaningfulAssetData(VendorAsset a) {
        return StringUtils.hasText(a.getPhoneModel())
                || StringUtils.hasText(a.getSimCard())
                || a.getIssuedOn() != null
                || StringUtils.hasText(a.getPhoneIssuedBy())
                || StringUtils.hasText(a.getSimIssuedBy());
    }

    private boolean isEventsPricingComplete(Long organizationId) {
        List<BridesideVendor> vendors = bridesideVendorRepository.findByOrganization_Id(organizationId);
        if (vendors.isEmpty()) return false;
        for (BridesideVendor vendor : vendors) {
            if (eventPricingRepository.countByVendor_Id(vendor.getId()) == 0) {
                return false;
            }
        }
        return true;
    }

    private boolean isVendorDataComplete(Long organizationId) {
        List<BridesideVendor> vendors = bridesideVendorRepository.findByOrganization_Id(organizationId);
        if (vendors.isEmpty()) return false;
        for (BridesideVendor vendor : vendors) {
            Optional<VendorData> vd = vendorDataRepository.findByVendor_Id(vendor.getId());
            if (vd.isEmpty()) return false;
            VendorData data = vd.get();
            boolean hasLink = StringUtils.hasText(data.getMasterDataLink()) || StringUtils.hasText(data.getCalendarSheetLink());
            if (!hasLink) return false;
        }
        return true;
    }

    private boolean isClientDataComplete(Long organizationId) {
        Optional<ClientData> cd = clientDataRepository.findByOrganization_Id(organizationId);
        if (cd.isEmpty()) return false;
        ClientData data = cd.get();
        return StringUtils.hasText(data.getQuoteFormatUrl()) && StringUtils.hasText(data.getClientContractFormatUrl());
    }

    private boolean isTeamMembersComplete(Long organizationId) {
        List<BridesideVendor> vendors = bridesideVendorRepository.findByOrganization_Id(organizationId);
        if (vendors.isEmpty()) return false;
        for (BridesideVendor vendor : vendors) {
            if (vendorTeamMemberRepository.countByVendor_Id(vendor.getId()) == 0) {
                return false;
            }
        }
        return true;
    }

    private OrganizationProgressDtos.ProgressResponse toResponse(Long orgId, OrganizationOnboardingProgress p, Boolean isActive) {
        OrganizationProgressDtos.ProgressResponse r = new OrganizationProgressDtos.ProgressResponse();
        r.setOrganizationId(orgId);
        r.setOrganizationDetailsComplete(p.getOrganizationDetailsComplete());
        r.setAssetInfoComplete(p.getAssetInfoComplete());
        r.setEventsPricingComplete(p.getEventsPricingComplete());
        r.setVendorDataComplete(p.getVendorDataComplete());
        r.setClientDataComplete(p.getClientDataComplete());
        r.setTeamMembersComplete(p.getTeamMembersComplete());
        r.setIsActive(Boolean.TRUE.equals(isActive));

        int completed = 0;
        if (Boolean.TRUE.equals(p.getOrganizationDetailsComplete())) completed++;
        if (Boolean.TRUE.equals(p.getAssetInfoComplete())) completed++;
        if (Boolean.TRUE.equals(p.getEventsPricingComplete())) completed++;
        if (Boolean.TRUE.equals(p.getVendorDataComplete())) completed++;
        if (Boolean.TRUE.equals(p.getClientDataComplete())) completed++;
        if (Boolean.TRUE.equals(p.getTeamMembersComplete())) completed++;

        r.setCompletedCount(completed);
        r.setTotalCount(TOTAL_SECTIONS);
        r.setUpdatedAt(p.getUpdatedAt());
        return r;
    }
}
