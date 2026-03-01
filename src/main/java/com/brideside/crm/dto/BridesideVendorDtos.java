package com.brideside.crm.dto;

import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Size;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public final class BridesideVendorDtos {
    private BridesideVendorDtos() {
    }

    public static class VendorCreateRequest {
        @Size(max = 100, message = "Username cannot exceed 100 characters")
        private String username;

        private Long pipelineId;

        @Size(max = 100, message = "IG account ID cannot exceed 100 characters")
        private String igAccountId;

        @Size(max = 50, message = "Business name cannot exceed 50 characters")
        private String businessName;

        @Size(max = 255, message = "Vendor name cannot exceed 255 characters")
        private String vendorName;

        /**
         * Stored in DB as JSON. Frontend can send an array/object string (e.g. ["Photography"]).
         */
        private String services;

        // Reuse editable fields at creation time as well
        @Size(max = 50, message = "Contact number cannot exceed 50 characters")
        private String contactNumber;

        @Size(max = 255, message = "Office/Studio location cannot exceed 255 characters")
        private String officeStudioLocation;

        @Size(max = 255, message = "Base location cannot exceed 255 characters")
        private String baseLocation;

        @Size(max = 50, message = "Official number cannot exceed 50 characters")
        private String officialNumber;

        @Email(message = "Email ID must be a valid email")
        @Size(max = 255, message = "Email ID cannot exceed 255 characters")
        private String emailId;

        private LocalDateTime onboardingDate;

        @Min(value = 0, message = "Team size cannot be negative")
        private Integer teamSize;

        @DecimalMin(value = "0.0", inclusive = true, message = "Onboarding fee cannot be negative")
        private BigDecimal onboardingFee;

        public String getUsername() { return username; }
        public void setUsername(String username) { this.username = username; }
        public Long getPipelineId() { return pipelineId; }
        public void setPipelineId(Long pipelineId) { this.pipelineId = pipelineId; }
        public String getIgAccountId() { return igAccountId; }
        public void setIgAccountId(String igAccountId) { this.igAccountId = igAccountId; }
        public String getBusinessName() { return businessName; }
        public void setBusinessName(String businessName) { this.businessName = businessName; }
        public String getVendorName() { return vendorName; }
        public void setVendorName(String vendorName) { this.vendorName = vendorName; }
        public String getServices() { return services; }
        public void setServices(String services) { this.services = services; }
        public String getContactNumber() { return contactNumber; }
        public void setContactNumber(String contactNumber) { this.contactNumber = contactNumber; }
        public String getOfficeStudioLocation() { return officeStudioLocation; }
        public void setOfficeStudioLocation(String officeStudioLocation) { this.officeStudioLocation = officeStudioLocation; }
        public String getBaseLocation() { return baseLocation; }
        public void setBaseLocation(String baseLocation) { this.baseLocation = baseLocation; }
        public String getOfficialNumber() { return officialNumber; }
        public void setOfficialNumber(String officialNumber) { this.officialNumber = officialNumber; }
        public String getEmailId() { return emailId; }
        public void setEmailId(String emailId) { this.emailId = emailId; }
        public LocalDateTime getOnboardingDate() { return onboardingDate; }
        public void setOnboardingDate(LocalDateTime onboardingDate) { this.onboardingDate = onboardingDate; }
        public Integer getTeamSize() { return teamSize; }
        public void setTeamSize(Integer teamSize) { this.teamSize = teamSize; }
        public BigDecimal getOnboardingFee() { return onboardingFee; }
        public void setOnboardingFee(BigDecimal onboardingFee) { this.onboardingFee = onboardingFee; }
    }

    public static class AccountOwnerSummary {
        private Long id;
        private String firstName;
        private String lastName;
        private String email;

        public Long getId() { return id; }
        public void setId(Long id) { this.id = id; }
        public String getFirstName() { return firstName; }
        public void setFirstName(String firstName) { this.firstName = firstName; }
        public String getLastName() { return lastName; }
        public void setLastName(String lastName) { this.lastName = lastName; }
        public String getEmail() { return email; }
        public void setEmail(String email) { this.email = email; }
    }

    public static class VendorResponse {
        private Long id;
        private String username;
        private Long organizationId;
        private Long pipelineId;
        private String igAccountId;
        private String businessName;
        private String vendorName;
        private String services;
        private AccountOwnerSummary accountOwner;

        private String contactNumber;
        private String officeStudioLocation;
        private String baseLocation;
        private String officialNumber;
        private String emailId;
        private LocalDateTime onboardingDate;
        private Integer teamSize;
        private BigDecimal onboardingFee;

        private LocalDateTime createdAt;
        private LocalDateTime updatedAt;

        public Long getId() { return id; }
        public void setId(Long id) { this.id = id; }
        public String getUsername() { return username; }
        public void setUsername(String username) { this.username = username; }
        public Long getOrganizationId() { return organizationId; }
        public void setOrganizationId(Long organizationId) { this.organizationId = organizationId; }
        public Long getPipelineId() { return pipelineId; }
        public void setPipelineId(Long pipelineId) { this.pipelineId = pipelineId; }
        public String getIgAccountId() { return igAccountId; }
        public void setIgAccountId(String igAccountId) { this.igAccountId = igAccountId; }
        public String getBusinessName() { return businessName; }
        public void setBusinessName(String businessName) { this.businessName = businessName; }
        public String getVendorName() { return vendorName; }
        public void setVendorName(String vendorName) { this.vendorName = vendorName; }
        public String getServices() { return services; }
        public void setServices(String services) { this.services = services; }
        public AccountOwnerSummary getAccountOwner() { return accountOwner; }
        public void setAccountOwner(AccountOwnerSummary accountOwner) { this.accountOwner = accountOwner; }
        public String getContactNumber() { return contactNumber; }
        public void setContactNumber(String contactNumber) { this.contactNumber = contactNumber; }
        public String getOfficeStudioLocation() { return officeStudioLocation; }
        public void setOfficeStudioLocation(String officeStudioLocation) { this.officeStudioLocation = officeStudioLocation; }
        public String getBaseLocation() { return baseLocation; }
        public void setBaseLocation(String baseLocation) { this.baseLocation = baseLocation; }
        public String getOfficialNumber() { return officialNumber; }
        public void setOfficialNumber(String officialNumber) { this.officialNumber = officialNumber; }
        public String getEmailId() { return emailId; }
        public void setEmailId(String emailId) { this.emailId = emailId; }
        public LocalDateTime getOnboardingDate() { return onboardingDate; }
        public void setOnboardingDate(LocalDateTime onboardingDate) { this.onboardingDate = onboardingDate; }
        public Integer getTeamSize() { return teamSize; }
        public void setTeamSize(Integer teamSize) { this.teamSize = teamSize; }
        public BigDecimal getOnboardingFee() { return onboardingFee; }
        public void setOnboardingFee(BigDecimal onboardingFee) { this.onboardingFee = onboardingFee; }
        public LocalDateTime getCreatedAt() { return createdAt; }
        public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }
        public LocalDateTime getUpdatedAt() { return updatedAt; }
        public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
    }

    public static class VendorUpdateRequest {
        private Long pipelineId;

        @Size(max = 255, message = "Vendor name cannot exceed 255 characters")
        private String vendorName;

        @Size(max = 50, message = "Contact number cannot exceed 50 characters")
        private String contactNumber;

        @Size(max = 255, message = "Office/Studio location cannot exceed 255 characters")
        private String officeStudioLocation;

        @Size(max = 255, message = "Base location cannot exceed 255 characters")
        private String baseLocation;

        @Size(max = 50, message = "Official number cannot exceed 50 characters")
        private String officialNumber;

        @Email(message = "Email ID must be a valid email")
        @Size(max = 255, message = "Email ID cannot exceed 255 characters")
        private String emailId;

        private LocalDateTime onboardingDate;

        @Min(value = 0, message = "Team size cannot be negative")
        private Integer teamSize;

        @DecimalMin(value = "0.0", inclusive = true, message = "Onboarding fee cannot be negative")
        private BigDecimal onboardingFee;

        public Long getPipelineId() { return pipelineId; }
        public void setPipelineId(Long pipelineId) { this.pipelineId = pipelineId; }
        public String getVendorName() { return vendorName; }
        public void setVendorName(String vendorName) { this.vendorName = vendorName; }
        public String getContactNumber() { return contactNumber; }
        public void setContactNumber(String contactNumber) { this.contactNumber = contactNumber; }
        public String getOfficeStudioLocation() { return officeStudioLocation; }
        public void setOfficeStudioLocation(String officeStudioLocation) { this.officeStudioLocation = officeStudioLocation; }
        public String getBaseLocation() { return baseLocation; }
        public void setBaseLocation(String baseLocation) { this.baseLocation = baseLocation; }
        public String getOfficialNumber() { return officialNumber; }
        public void setOfficialNumber(String officialNumber) { this.officialNumber = officialNumber; }
        public String getEmailId() { return emailId; }
        public void setEmailId(String emailId) { this.emailId = emailId; }
        public LocalDateTime getOnboardingDate() { return onboardingDate; }
        public void setOnboardingDate(LocalDateTime onboardingDate) { this.onboardingDate = onboardingDate; }
        public Integer getTeamSize() { return teamSize; }
        public void setTeamSize(Integer teamSize) { this.teamSize = teamSize; }
        public BigDecimal getOnboardingFee() { return onboardingFee; }
        public void setOnboardingFee(BigDecimal onboardingFee) { this.onboardingFee = onboardingFee; }
    }
}

