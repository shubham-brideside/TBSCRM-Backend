package com.brideside.crm.dto;

import jakarta.validation.constraints.Size;

import java.time.LocalDate;
import java.time.LocalDateTime;

public final class VendorAssetDtos {
    private VendorAssetDtos() {
    }

    public static class AssetResponse {
        private Long id;
        private Long vendorId;
        private Long organizationId;
        private String phoneModel;
        private String phoneIssuedBy;
        private String simCard;
        private String simIssuedBy;
        private LocalDate issuedOn;
        private LocalDateTime createdAt;
        private LocalDateTime updatedAt;

        public Long getId() { return id; }
        public void setId(Long id) { this.id = id; }
        public Long getVendorId() { return vendorId; }
        public void setVendorId(Long vendorId) { this.vendorId = vendorId; }
        public Long getOrganizationId() { return organizationId; }
        public void setOrganizationId(Long organizationId) { this.organizationId = organizationId; }
        public String getPhoneModel() { return phoneModel; }
        public void setPhoneModel(String phoneModel) { this.phoneModel = phoneModel; }
        public String getPhoneIssuedBy() { return phoneIssuedBy; }
        public void setPhoneIssuedBy(String phoneIssuedBy) { this.phoneIssuedBy = phoneIssuedBy; }
        public String getSimCard() { return simCard; }
        public void setSimCard(String simCard) { this.simCard = simCard; }
        public String getSimIssuedBy() { return simIssuedBy; }
        public void setSimIssuedBy(String simIssuedBy) { this.simIssuedBy = simIssuedBy; }
        public LocalDate getIssuedOn() { return issuedOn; }
        public void setIssuedOn(LocalDate issuedOn) { this.issuedOn = issuedOn; }
        public LocalDateTime getCreatedAt() { return createdAt; }
        public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }
        public LocalDateTime getUpdatedAt() { return updatedAt; }
        public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
    }

    public static class AssetUpdateRequest {
        @Size(max = 255, message = "Phone model cannot exceed 255 characters")
        private String phoneModel;

        @Size(max = 255, message = "Phone issued by cannot exceed 255 characters")
        private String phoneIssuedBy;

        @Size(max = 255, message = "SIM card cannot exceed 255 characters")
        private String simCard;

        @Size(max = 255, message = "SIM issued by cannot exceed 255 characters")
        private String simIssuedBy;

        private LocalDate issuedOn;

        public String getPhoneModel() { return phoneModel; }
        public void setPhoneModel(String phoneModel) { this.phoneModel = phoneModel; }
        public String getPhoneIssuedBy() { return phoneIssuedBy; }
        public void setPhoneIssuedBy(String phoneIssuedBy) { this.phoneIssuedBy = phoneIssuedBy; }
        public String getSimCard() { return simCard; }
        public void setSimCard(String simCard) { this.simCard = simCard; }
        public String getSimIssuedBy() { return simIssuedBy; }
        public void setSimIssuedBy(String simIssuedBy) { this.simIssuedBy = simIssuedBy; }
        public LocalDate getIssuedOn() { return issuedOn; }
        public void setIssuedOn(LocalDate issuedOn) { this.issuedOn = issuedOn; }
    }
}
