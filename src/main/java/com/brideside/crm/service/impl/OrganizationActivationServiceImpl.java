package com.brideside.crm.service.impl;

import com.brideside.crm.dto.OrganizationActivationDtos;
import com.brideside.crm.entity.BridesideVendor;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.OrganizationActivation;
import com.brideside.crm.entity.User;
import com.brideside.crm.repository.BridesideVendorRepository;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.OrganizationActivationRepository;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.service.EmailService;
import com.brideside.crm.service.OrganizationActivationService;
import com.fasterxml.jackson.databind.JsonNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

@Service
@Transactional
public class OrganizationActivationServiceImpl implements OrganizationActivationService {

    private static final Logger log = LoggerFactory.getLogger(OrganizationActivationServiceImpl.class);
    private static final int TOTAL_COUNT = 17;

    private final OrganizationActivationRepository activationRepository;
    private final BridesideVendorRepository bridesideVendorRepository;
    private final OrganizationRepository organizationRepository;
    private final EmailService emailService;

    private static final String EMAIL_DIVYANSHU = "divyanshu@acceltancy.in";
    private static final String EMAIL_SHUBHAM = "shubham@acceltancy.in";
    private static final String EMAIL_SHIVANI = "shivaniyadav@acceltancy.in";
    private static final String EMAIL_RAMNEET = "ramneet@acceltancy.in";
    private static final String EMAIL_MARKETING = "marketing@acceltancy.in";

    public OrganizationActivationServiceImpl(OrganizationActivationRepository activationRepository,
                                             BridesideVendorRepository bridesideVendorRepository,
                                             OrganizationRepository organizationRepository,
                                             EmailService emailService) {
        this.activationRepository = activationRepository;
        this.bridesideVendorRepository = bridesideVendorRepository;
        this.organizationRepository = organizationRepository;
        this.emailService = emailService;
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
        boolean firstActivationRowCreated = existing == null;
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
        syncVendorInstaIdIfProvided(organizationId, checklist, rawBody);

        if (firstActivationRowCreated) {
            sendMissingSectionEmailsAndMarkSent(saved, org);
        }

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
        activation.setInstaPassword(trimToNull(checklist.getInstaPassword()));
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

    private String trimToNull(String value) {
        if (value == null) return null;
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    private void syncVendorInstaIdIfProvided(Long organizationId, OrganizationActivationDtos.Checklist checklist, JsonNode rawBody) {
        if (rawBody == null || rawBody.isNull()) return;
        boolean instaIdProvided = rawBody.has("instaId") || rawBody.has("instagramId");
        if (!instaIdProvided) return;

        String incoming = null;
        if (rawBody.has("instaId")) {
            incoming = rawBody.get("instaId").isNull() ? null : rawBody.get("instaId").asText();
        } else if (rawBody.has("instagramId")) {
            incoming = rawBody.get("instagramId").isNull() ? null : rawBody.get("instagramId").asText();
        } else if (checklist != null) {
            incoming = checklist.getInstaId();
        }
        String normalized = trimToNull(incoming);
        if (normalized == null) {
            throw new BadRequestException("instaId cannot be blank");
        }

        BridesideVendor vendor = bridesideVendorRepository.findFirstByOrganization_IdOrderByIdAsc(organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("No brideside vendor found for organization id " + organizationId));
        vendor.setUsername(normalized);
        bridesideVendorRepository.save(vendor);
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
        checklist.setInstaId(resolveInstaId(activation));
        checklist.setInstaPassword(activation.getInstaPassword());

        response.setChecklist(checklist);
        return response;
    }

    private String resolveInstaId(OrganizationActivation activation) {
        if (activation == null || activation.getOrganization() == null || activation.getOrganization().getId() == null) {
            return null;
        }
        return bridesideVendorRepository.findFirstByOrganization_IdOrderByIdAsc(activation.getOrganization().getId())
                .map(BridesideVendor::getUsername)
                .orElse(null);
    }

    private void sendMissingSectionEmailsAndMarkSent(OrganizationActivation activation, Organization org) {
        if (activation == null || org == null) return;

        String orgName = org.getName();
        Long orgId = org.getId();

        User owner = org.getOwner();
        String ownerEmail = owner != null ? owner.getEmail() : null;

        User categoryManager = owner != null ? owner.getManager() : null;
        String categoryManagerEmail = categoryManager != null ? categoryManager.getEmail() : null;

        boolean contractSignedEffective = Boolean.TRUE.equals(activation.getVendorContract())
                || Boolean.TRUE.equals(activation.getContractSigned());

        boolean contractSectionMissing = !(
                contractSignedEffective
                        && Boolean.TRUE.equals(activation.getOnboardingFeeReceived())
                        && Boolean.TRUE.equals(activation.getGmailAndLoginCredentials())
                        && Boolean.TRUE.equals(activation.getPhoneAndSimIssued())
                        && Boolean.TRUE.equals(activation.getAdSetReady())
                        && Boolean.TRUE.equals(activation.getAdBudgetReady())
        );

        boolean calendarIgWhatsappSectionMissing = !(
                Boolean.TRUE.equals(activation.getCalendarSetup())
                        && Boolean.TRUE.equals(activation.getPricingSheetSetup())
                        && Boolean.TRUE.equals(activation.getIgContactDetails())
                        && Boolean.TRUE.equals(activation.getIgVet())
                        && Boolean.TRUE.equals(activation.getWhatsappSetup())
        );

        boolean hrSectionMissing = !(
                Boolean.TRUE.equals(activation.getHrContractVerification())
                        && Boolean.TRUE.equals(activation.getHrPhoneUndertakingForm())
        );

        boolean chatbotSectionMissing = !Boolean.TRUE.equals(activation.getChatbotSetup());

        boolean quoteClientContractSectionMissing = !(
                Boolean.TRUE.equals(activation.getQuoteReady())
                        && Boolean.TRUE.equals(activation.getClientContractReady())
        );

        boolean updated = false;

        // Contract section -> To: organization owner, CC: category manager + divyanshu + shubham
        if (contractSectionMissing && !Boolean.TRUE.equals(activation.getMailContractSectionSent())) {
            if (ownerEmail == null || ownerEmail.isBlank()) {
                log.warn("Skipping contract-section email: owner email missing for orgId={}", orgId);
            } else {
                List<String> cc = buildCcList(categoryManagerEmail, EMAIL_DIVYANSHU, EMAIL_SHUBHAM);
                String subject = "Brideside CRM - Missing Activation Checklist (Contract Section) - " + orgName;
                String plain = buildContractSectionPlain(orgId, orgName, contractSignedEffective, activation);
                String html = buildContractSectionHtml(orgId, orgName, contractSignedEffective, activation);
                try {
                    emailService.sendHtmlEmailWithCc(ownerEmail, cc, subject, html, plain);
                    activation.setMailContractSectionSent(true);
                    updated = true;
                } catch (Exception e) {
                    log.error("Failed to send contract-section email for orgId={}", orgId, e);
                }
            }
        }

        // Calendar/IG/WhatsApp section -> To: shivani, CC: category manager + divyanshu + shubham
        if (calendarIgWhatsappSectionMissing && !Boolean.TRUE.equals(activation.getMailCalendarIgWhatsappSectionSent())) {
            List<String> cc = buildCcList(categoryManagerEmail, EMAIL_DIVYANSHU, EMAIL_SHUBHAM);
            String subject = "Brideside CRM - Missing Activation Checklist (Calendar/IG/WhatsApp) - " + orgName;
            String plain = buildCalendarSectionPlain(orgId, orgName, activation);
            String html = buildCalendarSectionHtml(orgId, orgName, activation);
            try {
                emailService.sendHtmlEmailWithCc(EMAIL_SHIVANI, cc, subject, html, plain);
                activation.setMailCalendarIgWhatsappSectionSent(true);
                updated = true;
            } catch (Exception e) {
                log.error("Failed to send calendar/IG/WhatsApp-section email for orgId={}", orgId, e);
            }
        }

        // HR section -> To: ramneet, CC: divyanshu + shubham
        if (hrSectionMissing && !Boolean.TRUE.equals(activation.getMailHrSectionSent())) {
            List<String> cc = buildCcList(EMAIL_DIVYANSHU, EMAIL_SHUBHAM);
            String subject = "Brideside CRM - Missing Activation Checklist (HR) - " + orgName;
            String plain = buildHrSectionPlain(orgId, orgName, activation);
            String html = buildHrSectionHtml(orgId, orgName, activation);
            try {
                emailService.sendHtmlEmailWithCc(EMAIL_RAMNEET, cc, subject, html, plain);
                activation.setMailHrSectionSent(true);
                updated = true;
            } catch (Exception e) {
                log.error("Failed to send HR-section email for orgId={}", orgId, e);
            }
        }

        // Chatbot section -> To: shivani, CC: divyanshu + shubham
        if (chatbotSectionMissing && !Boolean.TRUE.equals(activation.getMailChatbotSectionSent())) {
            List<String> cc = buildCcList(EMAIL_DIVYANSHU, EMAIL_SHUBHAM);
            String subject = "Brideside CRM - Missing Activation Checklist (Chatbot) - " + orgName;
            String plain = buildChatbotSectionPlain(orgId, orgName, activation);
            String html = buildChatbotSectionHtml(orgId, orgName, activation);
            try {
                emailService.sendHtmlEmailWithCc(EMAIL_SHIVANI, cc, subject, html, plain);
                activation.setMailChatbotSectionSent(true);
                updated = true;
            } catch (Exception e) {
                log.error("Failed to send chatbot-section email for orgId={}", orgId, e);
            }
        }

        // Quote + client contract section -> To: marketing, CC: divyanshu + shubham
        if (quoteClientContractSectionMissing && !Boolean.TRUE.equals(activation.getMailQuoteClientContractSectionSent())) {
            List<String> cc = buildCcList(EMAIL_DIVYANSHU, EMAIL_SHUBHAM);
            String subject = "Brideside CRM - Missing Activation Checklist (Quote/Client Contract) - " + orgName;
            String plain = buildQuoteSectionPlain(orgId, orgName, activation);
            String html = buildQuoteSectionHtml(orgId, orgName, activation);
            try {
                emailService.sendHtmlEmailWithCc(EMAIL_MARKETING, cc, subject, html, plain);
                activation.setMailQuoteClientContractSectionSent(true);
                updated = true;
            } catch (Exception e) {
                log.error("Failed to send quote/client-contract-section email for orgId={}", orgId, e);
            }
        }

        if (updated) {
            activationRepository.save(activation);
        }
    }

    private static List<String> buildCcList(String... emails) {
        Set<String> out = new LinkedHashSet<>();
        if (emails == null) return List.of();
        for (String e : emails) {
            if (e != null) {
                String v = e.trim();
                if (!v.isEmpty()) out.add(v);
            }
        }
        return new ArrayList<>(out);
    }

    private static String boolVal(Boolean b) {
        return Boolean.TRUE.equals(b) ? "true" : "false";
    }

    private static String buildContractSectionPlain(Long orgId, String orgName, boolean contractSignedEffective, OrganizationActivation a) {
        return "Hi,\n\n" +
                "Missing activation checklist items (Contract Section) for organization: " + orgName + " (orgId=" + orgId + ").\n\n" +
                "- Contract signed: " + contractSignedEffective + "\n" +
                "- Onboarding fee received: " + boolVal(a.getOnboardingFeeReceived()) + "\n" +
                "- Gmail and login credentials: " + boolVal(a.getGmailAndLoginCredentials()) + "\n" +
                "- Phone and SIM card issued: " + boolVal(a.getPhoneAndSimIssued()) + "\n" +
                "- Ad set ready: " + boolVal(a.getAdSetReady()) + "\n" +
                "- Ad budget ready: " + boolVal(a.getAdBudgetReady()) + "\n\n" +
                "Please update the activation checklist in Brideside CRM.\n";
    }

    private static String buildContractSectionHtml(Long orgId, String orgName, boolean contractSignedEffective, OrganizationActivation a) {
        return "<p>Hi,</p>" +
                "<p>Missing activation checklist items <b>(Contract Section)</b> for organization: <b>" + orgName + "</b> (orgId=" + orgId + ").</p>" +
                "<ul>" +
                "<li>Contract signed: <b>" + contractSignedEffective + "</b></li>" +
                "<li>Onboarding fee received: <b>" + boolVal(a.getOnboardingFeeReceived()) + "</b></li>" +
                "<li>Gmail and login credentials: <b>" + boolVal(a.getGmailAndLoginCredentials()) + "</b></li>" +
                "<li>Phone and SIM card issued: <b>" + boolVal(a.getPhoneAndSimIssued()) + "</b></li>" +
                "<li>Ad set ready: <b>" + boolVal(a.getAdSetReady()) + "</b></li>" +
                "<li>Ad budget ready: <b>" + boolVal(a.getAdBudgetReady()) + "</b></li>" +
                "</ul>" +
                "<p>Please update the activation checklist in Brideside CRM.</p>";
    }

    private static String buildCalendarSectionPlain(Long orgId, String orgName, OrganizationActivation a) {
        return "Hi,\n\n" +
                "Missing activation checklist items (Calendar/IG/WhatsApp Section) for organization: " + orgName + " (orgId=" + orgId + ").\n\n" +
                "- Calendar setup: " + boolVal(a.getCalendarSetup()) + "\n" +
                "- Pricing sheet setup: " + boolVal(a.getPricingSheetSetup()) + "\n" +
                "- IG contract details: " + boolVal(a.getIgContactDetails()) + "\n" +
                "- IG Vet: " + boolVal(a.getIgVet()) + "\n" +
                "- Whatsapp setup: " + boolVal(a.getWhatsappSetup()) + "\n\n" +
                "Please update the activation checklist in Brideside CRM.\n";
    }

    private static String buildCalendarSectionHtml(Long orgId, String orgName, OrganizationActivation a) {
        return "<p>Hi,</p>" +
                "<p>Missing activation checklist items <b>(Calendar/IG/WhatsApp Section)</b> for organization: <b>" + orgName + "</b> (orgId=" + orgId + ").</p>" +
                "<ul>" +
                "<li>Calendar setup: <b>" + boolVal(a.getCalendarSetup()) + "</b></li>" +
                "<li>Pricing sheet setup: <b>" + boolVal(a.getPricingSheetSetup()) + "</b></li>" +
                "<li>IG contract details: <b>" + boolVal(a.getIgContactDetails()) + "</b></li>" +
                "<li>IG Vet: <b>" + boolVal(a.getIgVet()) + "</b></li>" +
                "<li>Whatsapp setup: <b>" + boolVal(a.getWhatsappSetup()) + "</b></li>" +
                "</ul>" +
                "<p>Please update the activation checklist in Brideside CRM.</p>";
    }

    private static String buildHrSectionPlain(Long orgId, String orgName, OrganizationActivation a) {
        return "Hi,\n\n" +
                "Missing activation checklist items (HR Section) for organization: " + orgName + " (orgId=" + orgId + ").\n\n" +
                "- HR Contract Verification: " + boolVal(a.getHrContractVerification()) + "\n" +
                "- HR Phone undertaking form: " + boolVal(a.getHrPhoneUndertakingForm()) + "\n\n" +
                "Please update the activation checklist in Brideside CRM.\n";
    }

    private static String buildHrSectionHtml(Long orgId, String orgName, OrganizationActivation a) {
        return "<p>Hi,</p>" +
                "<p>Missing activation checklist items <b>(HR Section)</b> for organization: <b>" + orgName + "</b> (orgId=" + orgId + ").</p>" +
                "<ul>" +
                "<li>HR Contract Verification: <b>" + boolVal(a.getHrContractVerification()) + "</b></li>" +
                "<li>HR Phone undertaking form: <b>" + boolVal(a.getHrPhoneUndertakingForm()) + "</b></li>" +
                "</ul>" +
                "<p>Please update the activation checklist in Brideside CRM.</p>";
    }

    private static String buildChatbotSectionPlain(Long orgId, String orgName, OrganizationActivation a) {
        return "Hi,\n\n" +
                "Chatbot setup is missing for organization: " + orgName + " (orgId=" + orgId + ").\n\n" +
                "- Chatbot setup: " + boolVal(a.getChatbotSetup()) + "\n\n" +
                "Please update the activation checklist in Brideside CRM.\n";
    }

    private static String buildChatbotSectionHtml(Long orgId, String orgName, OrganizationActivation a) {
        return "<p>Hi,</p>" +
                "<p>Chatbot setup is missing for organization: <b>" + orgName + "</b> (orgId=" + orgId + ").</p>" +
                "<ul>" +
                "<li>Chatbot setup: <b>" + boolVal(a.getChatbotSetup()) + "</b></li>" +
                "</ul>" +
                "<p>Please update the activation checklist in Brideside CRM.</p>";
    }

    private static String buildQuoteSectionPlain(Long orgId, String orgName, OrganizationActivation a) {
        return "Hi,\n\n" +
                "Missing activation checklist items (Quote/Client Contract Section) for organization: " + orgName + " (orgId=" + orgId + ").\n\n" +
                "- Quote ready: " + boolVal(a.getQuoteReady()) + "\n" +
                "- Client contract ready: " + boolVal(a.getClientContractReady()) + "\n\n" +
                "Please update the activation checklist in Brideside CRM.\n";
    }

    private static String buildQuoteSectionHtml(Long orgId, String orgName, OrganizationActivation a) {
        return "<p>Hi,</p>" +
                "<p>Missing activation checklist items <b>(Quote/Client Contract Section)</b> for organization: <b>" + orgName + "</b> (orgId=" + orgId + ").</p>" +
                "<ul>" +
                "<li>Quote ready: <b>" + boolVal(a.getQuoteReady()) + "</b></li>" +
                "<li>Client contract ready: <b>" + boolVal(a.getClientContractReady()) + "</b></li>" +
                "</ul>" +
                "<p>Please update the activation checklist in Brideside CRM.</p>";
    }
}

