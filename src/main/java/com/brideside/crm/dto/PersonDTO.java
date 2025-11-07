package com.brideside.crm.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;

@Schema(description = "Person data transfer object")
public class PersonDTO {
    @Schema(description = "Person ID (auto-generated, omit for POST requests)", type = "integer", format = "int64", accessMode = Schema.AccessMode.READ_ONLY)
    private Long id;
    
    @NotBlank
    @Schema(description = "Person name", required = true)
    private String name;
    
    @Schema(description = "Instagram ID")
    private String instagramId;
    
    @Schema(description = "Phone number")
    private String phone;
    
    @Schema(description = "Wedding date (format: dd/MM/yyyy)")
    private String weddingDate;
    
    @Schema(description = "Wedding venue")
    private String venue;
    
    @Schema(description = "Organization name")
    private String organization;
    
    @Schema(description = "Manager name")
    private String manager;
    
    @Schema(description = "Category")
    private String category;
    
    @Schema(description = "Source")
    private String source;
    
    @Schema(description = "Created date (format: dd/MM/yyyy)")
    private String createdDate;
    
    @Schema(description = "Event type")
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


