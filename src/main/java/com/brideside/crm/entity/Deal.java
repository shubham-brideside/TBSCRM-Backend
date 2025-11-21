package com.brideside.crm.entity;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Entity
@Table(name = "deals")
@NoArgsConstructor
@AllArgsConstructor
public class Deal {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String name;

    @Column(nullable = false, precision = 12, scale = 2)
    private BigDecimal value;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "person_id", nullable = true)
    private Person person;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "pipeline_id", nullable = true)
    private Pipeline pipeline;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "stage_id", nullable = true)
    private Stage stage;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "source_id")
    private Source source;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "organization_id", nullable = true)
    private Organization organization;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "category_id", nullable = true)
    private Category dealCategory;

    private String eventType; // free text per ER

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private DealStatus status = DealStatus.IN_PROGRESS;

    // Legacy column kept for backward DB compatibility until migration removes it
    @Column(name = "won", nullable = false)
    private Boolean legacyWon = false;

    // Compatibility with existing DB where deals has a non-null 'category' column
    @Column(nullable = true)
    private String category;

    // Compatibility with existing DB where deals has a non-null 'contact_number' column
    @Column(name = "contact_number", nullable = true)
    private String contactNumber;

    // Compatibility with existing DB where deals has a non-null 'user_name' column
    @Column(name = "user_name", nullable = true)
    private String userName;

    @Column(precision = 12, scale = 2)
    private BigDecimal commissionAmount; // computed on write

    // Additional optional fields
    @Column(name = "venue", nullable = true)
    private String venue;

    @Column(name = "phone_number", nullable = true)
    private String phoneNumber;

    @Column(name = "final_thank_you_sent", nullable = true)
    private Boolean finalThankYouSent;

    @Column(name = "event_date_asked", nullable = true)
    private Boolean eventDateAsked;

    @Column(name = "contact_number_asked", nullable = true)
    private Boolean contactNumberAsked;

    @Column(name = "venue_asked", nullable = true)
    private Boolean venueAsked;

    @Column(name = "event_date", nullable = true)
    private LocalDate eventDate;

    @Enumerated(EnumType.STRING)
    @Column(name = "label", length = 50, nullable = true)
    private DealLabel label;

    @Enumerated(EnumType.STRING)
    @Column(name = "deal_source", length = 50, nullable = true)
    private DealSource dealSource;

    @Column(name = "is_diverted", nullable = false)
    private Boolean isDiverted = Boolean.FALSE;

    @Column(name = "is_deleted", nullable = false)
    private Boolean isDeleted = Boolean.FALSE;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "referenced_deal_id", nullable = true)
    private Deal referencedDeal;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "referenced_pipeline_id", nullable = true)
    private Pipeline referencedPipeline;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "source_pipeline_id", nullable = true)
    private Pipeline sourcePipeline;

    @Column(name = "pipeline_history", columnDefinition = "JSON")
    private String pipelineHistory; // JSON array of pipeline IDs: [1, 2, 3]

    @CreationTimestamp
    @Column(nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = true)
    private LocalDateTime updatedAt;

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    public BigDecimal getValue() { return value; }
    public void setValue(BigDecimal value) { this.value = value; }
    public Person getPerson() { return person; }
    public void setPerson(Person person) { this.person = person; }
    public Pipeline getPipeline() { return pipeline; }
    public void setPipeline(Pipeline pipeline) { this.pipeline = pipeline; }
    public Stage getStage() { return stage; }
    public void setStage(Stage stage) { this.stage = stage; }
    public Source getSource() { return source; }
    public void setSource(Source source) { this.source = source; }
    public Organization getOrganization() { return organization; }
    public void setOrganization(Organization organization) { this.organization = organization; }
    public Category getDealCategory() { return dealCategory; }
    public void setDealCategory(Category dealCategory) { this.dealCategory = dealCategory; }
    public String getEventType() { return eventType; }
    public void setEventType(String eventType) { this.eventType = eventType; }
    public DealStatus getStatus() { return status; }
    public void setStatus(DealStatus status) { this.status = status; }
    public Boolean getLegacyWon() { return legacyWon; }
    public void setLegacyWon(Boolean legacyWon) { this.legacyWon = legacyWon; }
    public String getCategory() { return category; }
    public void setCategory(String category) { this.category = category; }
    public String getContactNumber() { return contactNumber; }
    public void setContactNumber(String contactNumber) { this.contactNumber = contactNumber; }
    public String getUserName() { return userName; }
    public void setUserName(String userName) { this.userName = userName; }
    public BigDecimal getCommissionAmount() { return commissionAmount; }
    public void setCommissionAmount(BigDecimal commissionAmount) { this.commissionAmount = commissionAmount; }
    public String getVenue() { return venue; }
    public void setVenue(String venue) { this.venue = venue; }
    public String getPhoneNumber() { return phoneNumber; }
    public void setPhoneNumber(String phoneNumber) { this.phoneNumber = phoneNumber; }
    public Boolean getFinalThankYouSent() { return finalThankYouSent; }
    public void setFinalThankYouSent(Boolean finalThankYouSent) { this.finalThankYouSent = finalThankYouSent; }
    public Boolean getEventDateAsked() { return eventDateAsked; }
    public void setEventDateAsked(Boolean eventDateAsked) { this.eventDateAsked = eventDateAsked; }
    public Boolean getContactNumberAsked() { return contactNumberAsked; }
    public void setContactNumberAsked(Boolean contactNumberAsked) { this.contactNumberAsked = contactNumberAsked; }
    public Boolean getVenueAsked() { return venueAsked; }
    public void setVenueAsked(Boolean venueAsked) { this.venueAsked = venueAsked; }
    public LocalDate getEventDate() { return eventDate; }
    public void setEventDate(LocalDate eventDate) { this.eventDate = eventDate; }
    public DealLabel getLabel() { return label; }
    public void setLabel(DealLabel label) { this.label = label; }
    public DealSource getDealSource() { return dealSource; }
    public void setDealSource(DealSource dealSource) { this.dealSource = dealSource; }
    public Boolean getIsDiverted() { return isDiverted; }
    public void setIsDiverted(Boolean isDiverted) { this.isDiverted = isDiverted; }
    public Boolean getIsDeleted() { return isDeleted; }
    public void setIsDeleted(Boolean isDeleted) { this.isDeleted = isDeleted; }
    public Deal getReferencedDeal() { return referencedDeal; }
    public void setReferencedDeal(Deal referencedDeal) { this.referencedDeal = referencedDeal; }
    public Pipeline getReferencedPipeline() { return referencedPipeline; }
    public void setReferencedPipeline(Pipeline referencedPipeline) { this.referencedPipeline = referencedPipeline; }
    public Pipeline getSourcePipeline() { return sourcePipeline; }
    public void setSourcePipeline(Pipeline sourcePipeline) { this.sourcePipeline = sourcePipeline; }
    public String getPipelineHistory() { return pipelineHistory; }
    public void setPipelineHistory(String pipelineHistory) { this.pipelineHistory = pipelineHistory; }
    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }
    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
    public String getGoogleCalendarEventId() { return googleCalendarEventId; }
    public void setGoogleCalendarEventId(String googleCalendarEventId) { this.googleCalendarEventId = googleCalendarEventId; }

    @PrePersist
    public void prePersist() {
        if (this.updatedAt == null) {
            this.updatedAt = this.createdAt;
        }
    }
}


