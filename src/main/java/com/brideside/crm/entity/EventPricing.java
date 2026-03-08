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
@Table(name = "events_pricing")
public class EventPricing {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "vendor_id", nullable = false)
    private BridesideVendor vendor;

    @Column(name = "session", nullable = false, length = 20)
    private String session;

    @Column(name = "artist_level", nullable = false, length = 20)
    private String artistLevel;

    @Column(name = "event_code", nullable = false, length = 100)
    private String eventCode;

    @Column(name = "event_label", length = 255)
    private String eventLabel;

    @Column(name = "base_price", precision = 12, scale = 2)
    private BigDecimal basePrice;

    @Column(name = "destination_price", precision = 12, scale = 2)
    private BigDecimal destinationPrice;

    @Column(name = "additional_makeup_price", precision = 12, scale = 2)
    private BigDecimal additionalMakeupPrice;

    @Column(name = "availability_at_studio", columnDefinition = "TEXT")
    private String availabilityAtStudio;

    @Column(name = "policy_notes", columnDefinition = "TEXT")
    private String policyNotes;

    @Column(name = "currency", length = 10)
    private String currency;

    @Column(name = "display_order")
    private Integer displayOrder;

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

    public String getSession() { return session; }
    public void setSession(String session) { this.session = session; }

    public String getArtistLevel() { return artistLevel; }
    public void setArtistLevel(String artistLevel) { this.artistLevel = artistLevel; }

    public String getEventCode() { return eventCode; }
    public void setEventCode(String eventCode) { this.eventCode = eventCode; }

    public String getEventLabel() { return eventLabel; }
    public void setEventLabel(String eventLabel) { this.eventLabel = eventLabel; }

    public BigDecimal getBasePrice() { return basePrice; }
    public void setBasePrice(BigDecimal basePrice) { this.basePrice = basePrice; }

    public BigDecimal getDestinationPrice() { return destinationPrice; }
    public void setDestinationPrice(BigDecimal destinationPrice) { this.destinationPrice = destinationPrice; }

    public BigDecimal getAdditionalMakeupPrice() { return additionalMakeupPrice; }
    public void setAdditionalMakeupPrice(BigDecimal additionalMakeupPrice) { this.additionalMakeupPrice = additionalMakeupPrice; }

    public String getAvailabilityAtStudio() { return availabilityAtStudio; }
    public void setAvailabilityAtStudio(String availabilityAtStudio) { this.availabilityAtStudio = availabilityAtStudio; }

    public String getPolicyNotes() { return policyNotes; }
    public void setPolicyNotes(String policyNotes) { this.policyNotes = policyNotes; }

    public String getCurrency() { return currency; }
    public void setCurrency(String currency) { this.currency = currency; }

    public Integer getDisplayOrder() { return displayOrder; }
    public void setDisplayOrder(Integer displayOrder) { this.displayOrder = displayOrder; }

    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }

    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
}
