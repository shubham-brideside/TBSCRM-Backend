package com.brideside.crm.dto;

import jakarta.validation.constraints.NotBlank;

public class PersonDTO {
    private Long id;
    @NotBlank
    private String name;
    private String instagramId;
    private String phone;
    private String weddingDate;
    private String venue;
    private String organization;
    private String manager;
    private String category;
    private String source;
    private String createdDate;
    private String eventType;

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }
    public String getInstagramId() { return instagramId; }
    public void setInstagramId(String instagramId) { this.instagramId = instagramId; }
    public String getPhone() { return phone; }
    public void setPhone(String phone) { this.phone = phone; }
    public String getWeddingDate() { return weddingDate; }
    public void setWeddingDate(String weddingDate) { this.weddingDate = weddingDate; }
    public String getVenue() { return venue; }
    public void setVenue(String venue) { this.venue = venue; }
    public String getOrganization() { return organization; }
    public void setOrganization(String organization) { this.organization = organization; }
    public String getManager() { return manager; }
    public void setManager(String manager) { this.manager = manager; }
    public String getCategory() { return category; }
    public void setCategory(String category) { this.category = category; }
    public String getSource() { return source; }
    public void setSource(String source) { this.source = source; }
    public String getCreatedDate() { return createdDate; }
    public void setCreatedDate(String createdDate) { this.createdDate = createdDate; }
    public String getEventType() { return eventType; }
    public void setEventType(String eventType) { this.eventType = eventType; }
}


