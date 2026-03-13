package com.brideside.crm.service.impl;

import com.brideside.crm.dto.OrganizationActivationDtos;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.OrganizationActivation;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.OrganizationActivationRepository;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.service.OrganizationActivationService;
import com.fasterxml.jackson.databind.JsonNode;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;

@Service
@Transactional
public class OrganizationActivationServiceImpl implements OrganizationActivationService {

    private static final int TOTAL_COUNT = 17;

    private final OrganizationActivationRepository activationRepository;
    private final OrganizationRepository organizationRepository;

    public OrganizationActivationServiceImpl(OrganizationActivationRepository activationRepository,
                                             OrganizationRepository organizationRepository) {
        this.activationRepository = activationRepository;
        this.organizationRepository = organizationRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public OrganizationActivationDtos.ProgressResponse getActivationProgress(Long organizationId) {
        validateOrganizationId(organizationId);
        return activationRepository.findByOrganization_Id(organizationId)
                .map(this::toProgressResponse)
                .orElseGet(() -> buildDefaultProgress(organizationId));
    }

    @Override
    public OrganizationActivationDtos.ProgressResponse saveChecklist(Long organizationId, OrganizationActivationDtos.Checklist checklist, JsonNode rawBody) {
        validateOrganizationId(organizationId);
        if (checklist == null) {
            checklist = new OrganizationActivationDtos.Checklist();
        }
        if (rawBody == null || rawBody.isNull()) {
            rawBody = com.fasterxml.jackson.databind.node.MissingNode.getInstance();
        }
        OrganizationActivation existing = activationRepository.findByOrganization_Id(organizationId).orElse(null);
        OrganizationActivation activation = existing != null ? existing : null;
        if (activation == null) {
            Organization org = organizationRepository.findById(organizationId)
                    .orElseThrow(() -> new ResourceNotFoundException("Organization not found with id " + organizationId));
            OrganizationActivation entity = new OrganizationActivation();
            entity.setOrganization(org);
            entity.setTotalCount(TOTAL_COUNT);
            activation = entity;
        }

        applyChecklistToEntity(activation, checklist, rawBody, existing);
        int completed = computeCompletedCount(activation);
        activation.setCompletedCount(completed);
        activation.setTotalCount(TOTAL_COUNT);
        // activated + org.is_active: only when these four are all true (not related to 17-item count)
        boolean coreFourDone = fourCoreComplete(activation);
        activation.setActivated(coreFourDone);

        OrganizationActivation saved = activationRepository.save(activation);
        Organization org = organizationRepository.findById(organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Organization not found with id " + organizationId));
        // organizations.is_active must match organization_activation.activated (1 / 0)
        org.setIsActive(Boolean.TRUE.equals(saved.getActivated()));
        organizationRepository.save(org);

        return toProgressResponse(saved);
    }

    private void validateOrganizationId(Long organizationId) {
        if (organizationId == null) {
            throw new BadRequestException("Organization id is required");
        }
        if (!organizationRepository.existsById(organizationId)) {
            throw new ResourceNotFoundException("Organization not found with id " + organizationId);
        }
    }

    private void applyChecklistToEntity(OrganizationActivation activation, OrganizationActivationDtos.Checklist checklist,
                                        JsonNode raw, OrganizationActivation previous) {
        // Vendor contract: must persist to vendor_contract only; accept vendorContract | contractSigned | vendor_contract in JSON
        boolean vendorContractInRequest = raw.hasNonNull("vendorContract") || raw.has("vendorContract")
                || raw.hasNonNull("contractSigned") || raw.has("contractSigned")
                || raw.hasNonNull("vendor_contract") || raw.has("vendor_contract");
        if (vendorContractInRequest) {
            boolean v = false;
            if (raw.has("vendorContract")) {
                v = raw.get("vendorContract").asBoolean(false);
            } else if (raw.has("contractSigned")) {
                v = raw.get("contractSigned").asBoolean(false);
            } else if (raw.has("vendor_contract")) {
                v = raw.get("vendor_contract").asBoolean(false);
            }
            activation.setVendorContract(v);
        } else if (previous != null) {
            activation.setVendorContract(Boolean.TRUE.equals(previous.getVendorContract()));
        } else {
            activation.setVendorContract(booleanOrFalse(checklist.getVendorContract()));
        }
        activation.setOnboardingFeeReceived(booleanOrFalse(checklist.getOnboardingFeeReceived()));
        activation.setGmailAndLoginCredentials(booleanOrFalse(checklist.getGmailAndLoginCredentials()));
        activation.setPhoneAndSimIssued(booleanOrFalse(checklist.getPhoneAndSimIssued()));
        activation.setCalendarSetup(booleanOrFalse(checklist.getCalendarSetup()));
        activation.setPricingSheetSetup(booleanOrFalse(checklist.getPricingSheetSetup()));
        activation.setIgContactDetails(booleanOrFalse(checklist.getIgContactDetails()));
        activation.setIgVet(booleanOrFalse(checklist.getIgVet()));
        activation.setWhatsappSetup(booleanOrFalse(checklist.getWhatsappSetup()));
        activation.setMasterData(booleanOrFalse(checklist.getMasterData()));
        activation.setHrContractVerification(booleanOrFalse(checklist.getHrContractVerification()));
        activation.setHrPhoneUndertakingForm(booleanOrFalse(checklist.getHrPhoneUndertakingForm()));
        activation.setChatbotSetup(booleanOrFalse(checklist.getChatbotSetup()));
        activation.setQuoteReady(booleanOrFalse(checklist.getQuoteReady()));
        activation.setClientContractReady(booleanOrFalse(checklist.getClientContractReady()));
        activation.setAdSetReady(booleanOrFalse(checklist.getAdSetReady()));
        activation.setAdBudgetReady(booleanOrFalse(checklist.getAdBudgetReady()));
    }

    private int computeCompletedCount(OrganizationActivation a) {
        int count = 0;
        if (Boolean.TRUE.equals(a.getVendorContract())) count++;
        if (Boolean.TRUE.equals(a.getOnboardingFeeReceived())) count++;
        if (Boolean.TRUE.equals(a.getGmailAndLoginCredentials())) count++;
        if (Boolean.TRUE.equals(a.getPhoneAndSimIssued())) count++;
        if (Boolean.TRUE.equals(a.getCalendarSetup())) count++;
        if (Boolean.TRUE.equals(a.getPricingSheetSetup())) count++;
        if (Boolean.TRUE.equals(a.getIgContactDetails())) count++;
        if (Boolean.TRUE.equals(a.getIgVet())) count++;
        if (Boolean.TRUE.equals(a.getWhatsappSetup())) count++;
        if (Boolean.TRUE.equals(a.getMasterData())) count++;
        if (Boolean.TRUE.equals(a.getHrContractVerification())) count++;
        if (Boolean.TRUE.equals(a.getHrPhoneUndertakingForm())) count++;
        if (Boolean.TRUE.equals(a.getChatbotSetup())) count++;
        if (Boolean.TRUE.equals(a.getQuoteReady())) count++;
        if (Boolean.TRUE.equals(a.getClientContractReady())) count++;
        if (Boolean.TRUE.equals(a.getAdSetReady())) count++;
        if (Boolean.TRUE.equals(a.getAdBudgetReady())) count++;
        return count;
    }

    private boolean booleanOrFalse(Boolean value) {
        return value != null && value;
    }

    /** Vendor contract + onboarding fee + Gmail/credentials + phone/SIM — drives activated + organizations.is_active only. */
    private boolean fourCoreComplete(OrganizationActivation a) {
        return Boolean.TRUE.equals(a.getVendorContract())
                && Boolean.TRUE.equals(a.getOnboardingFeeReceived())
                && Boolean.TRUE.equals(a.getGmailAndLoginCredentials())
                && Boolean.TRUE.equals(a.getPhoneAndSimIssued());
    }

    private OrganizationActivationDtos.ProgressResponse buildDefaultProgress(Long organizationId) {
        OrganizationActivationDtos.ProgressResponse response = new OrganizationActivationDtos.ProgressResponse();
        response.setOrganizationId(organizationId);
        response.setCompletedCount(0);
        response.setTotalCount(TOTAL_COUNT);
        response.setActivated(Boolean.FALSE);
        response.setUpdatedAt(LocalDateTime.now());

        OrganizationActivationDtos.Checklist checklist = new OrganizationActivationDtos.Checklist();
        response.setChecklist(checklist);
        return response;
    }

    private OrganizationActivationDtos.ProgressResponse toProgressResponse(OrganizationActivation activation) {
        OrganizationActivationDtos.ProgressResponse response = new OrganizationActivationDtos.ProgressResponse();
        response.setOrganizationId(
                activation.getOrganization() != null ? activation.getOrganization().getId() : null
        );
        response.setCompletedCount(activation.getCompletedCount());
        response.setTotalCount(activation.getTotalCount());
        response.setActivated(fourCoreComplete(activation));
        response.setUpdatedAt(activation.getUpdatedAt());

        OrganizationActivationDtos.Checklist checklist = new OrganizationActivationDtos.Checklist();
        checklist.setVendorContract(activation.getVendorContract());
        checklist.setOnboardingFeeReceived(activation.getOnboardingFeeReceived());
        checklist.setGmailAndLoginCredentials(activation.getGmailAndLoginCredentials());
        checklist.setPhoneAndSimIssued(activation.getPhoneAndSimIssued());
        checklist.setCalendarSetup(activation.getCalendarSetup());
        checklist.setPricingSheetSetup(activation.getPricingSheetSetup());
        checklist.setIgContactDetails(activation.getIgContactDetails());
        checklist.setIgVet(activation.getIgVet());
        checklist.setWhatsappSetup(activation.getWhatsappSetup());
        checklist.setMasterData(activation.getMasterData());
        checklist.setHrContractVerification(activation.getHrContractVerification());
        checklist.setHrPhoneUndertakingForm(activation.getHrPhoneUndertakingForm());
        checklist.setChatbotSetup(activation.getChatbotSetup());
        checklist.setQuoteReady(activation.getQuoteReady());
        checklist.setClientContractReady(activation.getClientContractReady());
        checklist.setAdSetReady(activation.getAdSetReady());
        checklist.setAdBudgetReady(activation.getAdBudgetReady());

        response.setChecklist(checklist);
        return response;
    }
}

