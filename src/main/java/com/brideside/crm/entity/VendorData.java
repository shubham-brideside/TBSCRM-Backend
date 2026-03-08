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
@Table(name = "vendor_data")
public class VendorData {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "vendor_id", nullable = false, unique = true)
    private BridesideVendor vendor;

    @Column(name = "master_data_link", columnDefinition = "TEXT")
    private String masterDataLink;

    @Column(name = "calendar_sheet_link", columnDefinition = "TEXT")
    private String calendarSheetLink;

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

    public String getMasterDataLink() { return masterDataLink; }
    public void setMasterDataLink(String masterDataLink) { this.masterDataLink = masterDataLink; }

    public String getCalendarSheetLink() { return calendarSheetLink; }
    public void setCalendarSheetLink(String calendarSheetLink) { this.calendarSheetLink = calendarSheetLink; }

    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }

    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
}
