package com.brideside.crm.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.MapsId;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;

@Entity
@Table(name = "organization_onboarding_progress")
public class OrganizationOnboardingProgress {

    @Id
    private Long id;

    @OneToOne(fetch = FetchType.LAZY)
    @MapsId
    @JoinColumn(name = "organization_id", nullable = false)
    private Organization organization;

    @Column(name = "organization_details_complete", nullable = false)
    private Boolean organizationDetailsComplete = Boolean.FALSE;

    @Column(name = "asset_info_complete", nullable = false)
    private Boolean assetInfoComplete = Boolean.FALSE;

    @Column(name = "events_pricing_complete", nullable = false)
    private Boolean eventsPricingComplete = Boolean.FALSE;

    @Column(name = "vendor_data_complete", nullable = false)
    private Boolean vendorDataComplete = Boolean.FALSE;

    @Column(name = "client_data_complete", nullable = false)
    private Boolean clientDataComplete = Boolean.FALSE;

    @Column(name = "team_members_complete", nullable = false)
    private Boolean teamMembersComplete = Boolean.FALSE;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public Organization getOrganization() { return organization; }
    public void setOrganization(Organization organization) { this.organization = organization; }

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

    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
}
