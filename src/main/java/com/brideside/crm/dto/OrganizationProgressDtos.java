package com.brideside.crm.dto;

import java.time.LocalDateTime;

public final class OrganizationProgressDtos {

    private OrganizationProgressDtos() {
    }

    public static class ProgressResponse {
        private Long organizationId;
        private Boolean organizationDetailsComplete;
        private Boolean assetInfoComplete;
        private Boolean eventsPricingComplete;
        private Boolean vendorDataComplete;
        private Boolean clientDataComplete;
        private Boolean teamMembersComplete;
        private Boolean isActive;
        private Integer completedCount;
        private Integer totalCount;
        private LocalDateTime updatedAt;

        public Long getOrganizationId() { return organizationId; }
        public void setOrganizationId(Long organizationId) { this.organizationId = organizationId; }

        public Boolean getOrganizationDetailsComplete() { return organizationDetailsComplete; }
        public void setOrganizationDetailsComplete(Boolean organizationDetailsComplete) { this.organizationDetailsComplete = organizationDetailsComplete; }

        public Boolean getAssetInfoComplete() { return assetInfoComplete; }
        public void setAssetInfoComplete(Boolean assetInfoComplete) { this.assetInfoComplete = assetInfoComplete; }

        public Boolean getEventsPricingComplete() { return eventsPricingComplete; }
        public void setEventsPricingComplete(Boolean eventsPricingComplete) { this.eventsPricingComplete = eventsPricingComplete; }

        public Boolean getVendorDataComplete() { return vendorDataComplete; }
        public void setVendorDataComplete(Boolean vendorDataComplete) { this.vendorDataComplete = vendorDataComplete; }

        public Boolean getClientDataComplete() { return clientDataComplete; }
        public void setClientDataComplete(Boolean clientDataComplete) { this.clientDataComplete = clientDataComplete; }

        public Boolean getTeamMembersComplete() { return teamMembersComplete; }
        public void setTeamMembersComplete(Boolean teamMembersComplete) { this.teamMembersComplete = teamMembersComplete; }

        public Boolean getIsActive() { return isActive; }
        public void setIsActive(Boolean isActive) { this.isActive = isActive; }

        public Integer getCompletedCount() { return completedCount; }
        public void setCompletedCount(Integer completedCount) { this.completedCount = completedCount; }

        public Integer getTotalCount() { return totalCount; }
        public void setTotalCount(Integer totalCount) { this.totalCount = totalCount; }

        public LocalDateTime getUpdatedAt() { return updatedAt; }
        public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
    }

    /** Debug response: includes raw client data to diagnose clientDataComplete. */
    public static class ProgressDebugResponse extends ProgressResponse {
        private Boolean clientDataRecordExists;
        private Boolean quoteFormatUrlPresent;
        private Boolean clientContractFormatUrlPresent;

        public Boolean getClientDataRecordExists() { return clientDataRecordExists; }
        public void setClientDataRecordExists(Boolean clientDataRecordExists) { this.clientDataRecordExists = clientDataRecordExists; }
        public Boolean getQuoteFormatUrlPresent() { return quoteFormatUrlPresent; }
        public void setQuoteFormatUrlPresent(Boolean quoteFormatUrlPresent) { this.quoteFormatUrlPresent = quoteFormatUrlPresent; }
        public Boolean getClientContractFormatUrlPresent() { return clientContractFormatUrlPresent; }
        public void setClientContractFormatUrlPresent(Boolean clientContractFormatUrlPresent) { this.clientContractFormatUrlPresent = clientContractFormatUrlPresent; }
    }
}
