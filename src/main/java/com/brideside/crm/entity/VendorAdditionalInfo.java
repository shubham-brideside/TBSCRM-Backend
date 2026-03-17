package com.brideside.crm.entity;

import jakarta.persistence.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "vendor_additional_info")
public class VendorAdditionalInfo {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "vendor_id", nullable = false, unique = true)
    private BridesideVendor vendor;

    @Column(name = "starting_price_two_day_wedding", length = 100)
    private String startingPriceTwoDayWedding;

    @Column(name = "wedding_per_day", length = 100)
    private String weddingPerDay;

    @Column(name = "turnaround_time", length = 200)
    private String turnaroundTime;

    @Column(name = "photography_style", length = 500)
    private String photographyStyle;

    @Column(name = "travel_accommodation_separate", length = 10)
    private String travelAccommodationSeparate;

    @Column(name = "vendor_contract_url", length = 1024)
    private String vendorContractUrl;

    @Column(name = "vendor_logo_url", length = 1024)
    private String vendorLogoUrl;

    @OneToMany(mappedBy = "additionalInfo", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<VendorAdditionalCustomField> customFields = new ArrayList<>();

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

    public String getStartingPriceTwoDayWedding() { return startingPriceTwoDayWedding; }
    public void setStartingPriceTwoDayWedding(String startingPriceTwoDayWedding) { this.startingPriceTwoDayWedding = startingPriceTwoDayWedding; }

    public String getWeddingPerDay() { return weddingPerDay; }
    public void setWeddingPerDay(String weddingPerDay) { this.weddingPerDay = weddingPerDay; }

    public String getTurnaroundTime() { return turnaroundTime; }
    public void setTurnaroundTime(String turnaroundTime) { this.turnaroundTime = turnaroundTime; }

    public String getPhotographyStyle() { return photographyStyle; }
    public void setPhotographyStyle(String photographyStyle) { this.photographyStyle = photographyStyle; }

    public String getTravelAccommodationSeparate() { return travelAccommodationSeparate; }
    public void setTravelAccommodationSeparate(String travelAccommodationSeparate) { this.travelAccommodationSeparate = travelAccommodationSeparate; }

    public String getVendorContractUrl() { return vendorContractUrl; }
    public void setVendorContractUrl(String vendorContractUrl) { this.vendorContractUrl = vendorContractUrl; }

    public String getVendorLogoUrl() { return vendorLogoUrl; }
    public void setVendorLogoUrl(String vendorLogoUrl) { this.vendorLogoUrl = vendorLogoUrl; }

    public List<VendorAdditionalCustomField> getCustomFields() { return customFields; }
    public void setCustomFields(List<VendorAdditionalCustomField> customFields) { this.customFields = customFields; }

    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }

    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
}

