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

import java.time.LocalDate;
import java.time.LocalDateTime;

@Entity
@Table(name = "vendor_assets")
public class VendorAsset {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "vendor_id", nullable = false)
    private BridesideVendor vendor;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "organization_id")
    private Organization organization;

    @Column(name = "phone_model", length = 255)
    private String phoneModel;

    @Column(name = "phone_issued_by", length = 255)
    private String phoneIssuedBy;

    @Column(name = "sim_card", length = 255)
    private String simCard;

    @Column(name = "sim_issued_by", length = 255)
    private String simIssuedBy;

    @Column(name = "issued_on")
    private LocalDate issuedOn;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public BridesideVendor getVendor() { return vendor; }
    public void setVendor(BridesideVendor vendor) { this.vendor = vendor; }

    public Organization getOrganization() { return organization; }
    public void setOrganization(Organization organization) { this.organization = organization; }

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
