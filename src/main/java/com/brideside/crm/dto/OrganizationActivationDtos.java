package com.brideside.crm.dto;

import com.fasterxml.jackson.annotation.JsonAlias;

import java.time.LocalDateTime;

public final class OrganizationActivationDtos {

    private OrganizationActivationDtos() {
    }

    public static class Checklist {
        private Boolean vendorContract;
        private Boolean onboardingFeeReceived;
        private Boolean gmailAndLoginCredentials;
        private Boolean phoneAndSimIssued;
        private Boolean calendarSetup;
        private Boolean pricingSheetSetup;
        private Boolean igContactDetails;
        private Boolean igVet;
        private Boolean whatsappSetup;
        private Boolean masterData;
        private Boolean hrContractVerification;
        private Boolean hrPhoneUndertakingForm;
        private Boolean chatbotSetup;
        private Boolean quoteReady;
        private Boolean clientContractReady;
        private Boolean adSetReady;
        private Boolean adBudgetReady;

        public Boolean getVendorContract() { return vendorContract; }
        public void setVendorContract(Boolean vendorContract) { this.vendorContract = vendorContract; }
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
    }

    public static class ProgressResponse {
        private Long organizationId;
        private Integer completedCount;
        private Integer totalCount;
        private Boolean activated;
        private LocalDateTime updatedAt;
        private Checklist checklist;

        public Long getOrganizationId() { return organizationId; }
        public void setOrganizationId(Long organizationId) { this.organizationId = organizationId; }
        public Integer getCompletedCount() { return completedCount; }
        public void setCompletedCount(Integer completedCount) { this.completedCount = completedCount; }
        public Integer getTotalCount() { return totalCount; }
        public void setTotalCount(Integer totalCount) { this.totalCount = totalCount; }
        public Boolean getActivated() { return activated; }
        public void setActivated(Boolean activated) { this.activated = activated; }
        public LocalDateTime getUpdatedAt() { return updatedAt; }
        public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
        public Checklist getChecklist() { return checklist; }
        public void setChecklist(Checklist checklist) { this.checklist = checklist; }
    }
}

