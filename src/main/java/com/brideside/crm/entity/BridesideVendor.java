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

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Entity
@Table(name = "brideside_vendors")
public class BridesideVendor {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, length = 100, unique = true)
    private String username;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "organization_id", nullable = false)
    private Organization organization;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "pipeline_id", nullable = false)
    private Pipeline pipeline;

    @Column(name = "ig_account_id", length = 100, unique = true)
    private String igAccountId;

    @Column(name = "access_token", columnDefinition = "TEXT")
    private String accessToken;

    @Column(name = "business_name", length = 50)
    private String businessName;

    @Column(name = "vendor_name", length = 255)
    private String vendorName;

    @Column(columnDefinition = "TEXT")
    private String about;

    @Column(columnDefinition = "JSON")
    private String services;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "account_owner")
    private User accountOwner;

    @Column(name = "contact_number", length = 50)
    private String contactNumber;

    @Column(name = "office_studio_location", length = 255)
    private String officeStudioLocation;

    @Column(name = "base_location", length = 255)
    private String baseLocation;

    @Column(name = "official_number", length = 50)
    private String officialNumber;

    @Column(name = "email_id", length = 255)
    private String emailId;

    @Column(name = "onboarding_date")
    private LocalDateTime onboardingDate;

    @Column(name = "team_size")
    private Integer teamSize;

    @Column(name = "onboarding_fee", precision = 12, scale = 2)
    private BigDecimal onboardingFee;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public String getUsername() { return username; }
    public void setUsername(String username) { this.username = username; }

    public Organization getOrganization() { return organization; }
    public void setOrganization(Organization organization) { this.organization = organization; }

    public Pipeline getPipeline() { return pipeline; }
    public void setPipeline(Pipeline pipeline) { this.pipeline = pipeline; }

    public String getIgAccountId() { return igAccountId; }
    public void setIgAccountId(String igAccountId) { this.igAccountId = igAccountId; }

    public String getAccessToken() { return accessToken; }
    public void setAccessToken(String accessToken) { this.accessToken = accessToken; }

    public String getBusinessName() { return businessName; }
    public void setBusinessName(String businessName) { this.businessName = businessName; }

    public String getVendorName() { return vendorName; }
    public void setVendorName(String vendorName) { this.vendorName = vendorName; }

    public String getAbout() { return about; }
    public void setAbout(String about) { this.about = about; }

    public String getServices() { return services; }
    public void setServices(String services) { this.services = services; }

    public User getAccountOwner() { return accountOwner; }
    public void setAccountOwner(User accountOwner) { this.accountOwner = accountOwner; }

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

