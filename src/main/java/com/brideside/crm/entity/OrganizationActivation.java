package com.brideside.crm.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;

@Entity
@Table(name = "organization_activation")
public class OrganizationActivation {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "organization_id", nullable = false, unique = true)
    private Organization organization;

    /** UI "Vendor contract". */
    @Column(name = "vendor_contract", nullable = false)
    private Boolean vendorContract = Boolean.FALSE;

    /**
     * Same meaning as {@link #vendorContract}. Still present on some DBs as NOT NULL with no default;
     * Hibernate must set it on INSERT or MySQL errors. Kept in sync on every save.
     */
    @Column(name = "contract_signed", nullable = false)
    private Boolean contractSigned = Boolean.FALSE;

    @Column(name = "onboarding_fee_received", nullable = false)
    private Boolean onboardingFeeReceived = Boolean.FALSE;

    @Column(name = "gmail_and_login_credentials", nullable = false)
    private Boolean gmailAndLoginCredentials = Boolean.FALSE;

    @Column(name = "phone_and_sim_issued", nullable = false)
    private Boolean phoneAndSimIssued = Boolean.FALSE;

    @Column(name = "calendar_setup", nullable = false)
    private Boolean calendarSetup = Boolean.FALSE;

    @Column(name = "pricing_sheet_setup", nullable = false)
    private Boolean pricingSheetSetup = Boolean.FALSE;

    @Column(name = "ig_contact_details", nullable = false)
    private Boolean igContactDetails = Boolean.FALSE;

    @Column(name = "ig_vet", nullable = false)
    private Boolean igVet = Boolean.FALSE;

    @Column(name = "whatsapp_setup", nullable = false)
    private Boolean whatsappSetup = Boolean.FALSE;

    @Column(name = "master_data", nullable = false)
    private Boolean masterData = Boolean.FALSE;

    @Column(name = "hr_contract_verification", nullable = false)
    private Boolean hrContractVerification = Boolean.FALSE;

    @Column(name = "hr_phone_undertaking_form", nullable = false)
    private Boolean hrPhoneUndertakingForm = Boolean.FALSE;

    @Column(name = "chatbot_setup", nullable = false)
    private Boolean chatbotSetup = Boolean.FALSE;

    @Column(name = "quote_ready", nullable = false)
    private Boolean quoteReady = Boolean.FALSE;

    @Column(name = "client_contract_ready", nullable = false)
    private Boolean clientContractReady = Boolean.FALSE;

    @Column(name = "ad_set_ready", nullable = false)
    private Boolean adSetReady = Boolean.FALSE;

    @Column(name = "ad_budget_ready", nullable = false)
    private Boolean adBudgetReady = Boolean.FALSE;

    @Column(name = "insta_password", length = 255)
    private String instaPassword;

    @Column(name = "completed_count", nullable = false)
    private Integer completedCount = 0;

    @Column(name = "total_count", nullable = false)
    private Integer totalCount = 17;

    @Column(name = "activated", nullable = false)
    private Boolean activated = Boolean.FALSE;

    @Column(name = "mail_contract_section_sent", nullable = false)
    private Boolean mailContractSectionSent = Boolean.FALSE;

    @Column(name = "mail_calendar_ig_whatsapp_section_sent", nullable = false)
    private Boolean mailCalendarIgWhatsappSectionSent = Boolean.FALSE;

    @Column(name = "mail_hr_section_sent", nullable = false)
    private Boolean mailHrSectionSent = Boolean.FALSE;

    @Column(name = "mail_chatbot_section_sent", nullable = false)
    private Boolean mailChatbotSectionSent = Boolean.FALSE;

    @Column(name = "mail_quote_client_contract_section_sent", nullable = false)
    private Boolean mailQuoteClientContractSectionSent = Boolean.FALSE;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public Organization getOrganization() { return organization; }
    public void setOrganization(Organization organization) { this.organization = organization; }

    public Boolean getVendorContract() { return vendorContract; }
    public void setVendorContract(Boolean vendorContract) {
        this.vendorContract = vendorContract != null && vendorContract;
        this.contractSigned = this.vendorContract;
    }

    public Boolean getContractSigned() { return contractSigned; }
    public void setContractSigned(Boolean contractSigned) {
        boolean v = contractSigned != null && contractSigned;
        this.contractSigned = v;
        this.vendorContract = v;
    }

    public Boolean getOnboardingFeeReceived() { return onboardingFeeReceived; }
    public void setOnboardingFeeReceived(Boolean onboardingFeeReceived) { this.onboardingFeeReceived = onboardingFeeReceived; }

    public Boolean getGmailAndLoginCredentials() { return gmailAndLoginCredentials; }
    public void setGmailAndLoginCredentials(Boolean gmailAndLoginCredentials) { this.gmailAndLoginCredentials = gmailAndLoginCredentials; }

    public Boolean getPhoneAndSimIssued() { return phoneAndSimIssued; }
    public void setPhoneAndSimIssued(Boolean phoneAndSimIssued) { this.phoneAndSimIssued = phoneAndSimIssued; }

    public Boolean getCalendarSetup() { return calendarSetup; }
    public void setCalendarSetup(Boolean calendarSetup) { this.calendarSetup = calendarSetup; }

    public Boolean getPricingSheetSetup() { return pricingSheetSetup; }
    public void setPricingSheetSetup(Boolean pricingSheetSetup) { this.pricingSheetSetup = pricingSheetSetup; }

    public Boolean getIgContactDetails() { return igContactDetails; }
    public void setIgContactDetails(Boolean igContactDetails) { this.igContactDetails = igContactDetails; }

    public Boolean getIgVet() { return igVet; }
    public void setIgVet(Boolean igVet) { this.igVet = igVet; }

    public Boolean getWhatsappSetup() { return whatsappSetup; }
    public void setWhatsappSetup(Boolean whatsappSetup) { this.whatsappSetup = whatsappSetup; }

    public Boolean getMasterData() { return masterData; }
    public void setMasterData(Boolean masterData) { this.masterData = masterData; }

    public Boolean getHrContractVerification() { return hrContractVerification; }
    public void setHrContractVerification(Boolean hrContractVerification) { this.hrContractVerification = hrContractVerification; }

    public Boolean getHrPhoneUndertakingForm() { return hrPhoneUndertakingForm; }
    public void setHrPhoneUndertakingForm(Boolean hrPhoneUndertakingForm) { this.hrPhoneUndertakingForm = hrPhoneUndertakingForm; }

    public Boolean getChatbotSetup() { return chatbotSetup; }
    public void setChatbotSetup(Boolean chatbotSetup) { this.chatbotSetup = chatbotSetup; }

    public Boolean getQuoteReady() { return quoteReady; }
    public void setQuoteReady(Boolean quoteReady) { this.quoteReady = quoteReady; }

    public Boolean getClientContractReady() { return clientContractReady; }
    public void setClientContractReady(Boolean clientContractReady) { this.clientContractReady = clientContractReady; }

    public Boolean getAdSetReady() { return adSetReady; }
    public void setAdSetReady(Boolean adSetReady) { this.adSetReady = adSetReady; }

    public Boolean getAdBudgetReady() { return adBudgetReady; }
    public void setAdBudgetReady(Boolean adBudgetReady) { this.adBudgetReady = adBudgetReady; }

    public String getInstaPassword() { return instaPassword; }
    public void setInstaPassword(String instaPassword) { this.instaPassword = instaPassword; }

    public Integer getCompletedCount() { return completedCount; }
    public void setCompletedCount(Integer completedCount) { this.completedCount = completedCount; }

    public Integer getTotalCount() { return totalCount; }
    public void setTotalCount(Integer totalCount) { this.totalCount = totalCount; }

    public Boolean getActivated() { return activated; }
    public void setActivated(Boolean activated) { this.activated = activated; }

    public Boolean getMailContractSectionSent() { return mailContractSectionSent; }
    public void setMailContractSectionSent(Boolean mailContractSectionSent) {
        this.mailContractSectionSent = mailContractSectionSent != null && mailContractSectionSent;
    }

    public Boolean getMailCalendarIgWhatsappSectionSent() { return mailCalendarIgWhatsappSectionSent; }
    public void setMailCalendarIgWhatsappSectionSent(Boolean mailCalendarIgWhatsappSectionSent) {
        this.mailCalendarIgWhatsappSectionSent = mailCalendarIgWhatsappSectionSent != null && mailCalendarIgWhatsappSectionSent;
    }

    public Boolean getMailHrSectionSent() { return mailHrSectionSent; }
    public void setMailHrSectionSent(Boolean mailHrSectionSent) {
        this.mailHrSectionSent = mailHrSectionSent != null && mailHrSectionSent;
    }

    public Boolean getMailChatbotSectionSent() { return mailChatbotSectionSent; }
    public void setMailChatbotSectionSent(Boolean mailChatbotSectionSent) {
        this.mailChatbotSectionSent = mailChatbotSectionSent != null && mailChatbotSectionSent;
    }

    public Boolean getMailQuoteClientContractSectionSent() { return mailQuoteClientContractSectionSent; }
    public void setMailQuoteClientContractSectionSent(Boolean mailQuoteClientContractSectionSent) {
        this.mailQuoteClientContractSectionSent = mailQuoteClientContractSectionSent != null && mailQuoteClientContractSectionSent;
    }

    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }

    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
}

