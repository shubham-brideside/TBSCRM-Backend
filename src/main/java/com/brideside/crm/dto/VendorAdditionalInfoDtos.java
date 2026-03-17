package com.brideside.crm.dto;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

public final class VendorAdditionalInfoDtos {

    private VendorAdditionalInfoDtos() {
    }

    public static class CustomFieldResponse {
        private Long id;
        private String label;
        private String type;
        private String value;
        private LocalDateTime createdAt;
        private LocalDateTime updatedAt;

        public Long getId() { return id; }
        public void setId(Long id) { this.id = id; }
        public String getLabel() { return label; }
        public void setLabel(String label) { this.label = label; }
        public String getType() { return type; }
        public void setType(String type) { this.type = type; }
        public String getValue() { return value; }
        public void setValue(String value) { this.value = value; }
        public LocalDateTime getCreatedAt() { return createdAt; }
        public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }
        public LocalDateTime getUpdatedAt() { return updatedAt; }
        public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
    }

    public static class AdditionalInfoResponse {
        private Long id;
        private Long vendorId;
        private String startingPriceTwoDayWedding;
        private String weddingPerDay;
        private String turnaroundTime;
        private String photographyStyle;
        private String travelAccommodationSeparate;
        private String vendorContractUrl;
        private String vendorLogoUrl;
        private List<CustomFieldResponse> customFields = new ArrayList<>();
        private LocalDateTime createdAt;
        private LocalDateTime updatedAt;

        public Long getId() { return id; }
        public void setId(Long id) { this.id = id; }
        public Long getVendorId() { return vendorId; }
        public void setVendorId(Long vendorId) { this.vendorId = vendorId; }
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
        public List<CustomFieldResponse> getCustomFields() { return customFields; }
        public void setCustomFields(List<CustomFieldResponse> customFields) { this.customFields = customFields; }
        public LocalDateTime getCreatedAt() { return createdAt; }
        public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }
        public LocalDateTime getUpdatedAt() { return updatedAt; }
        public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
    }

    public static class CustomFieldRequest {

        @Size(max = 255, message = "Custom field label cannot exceed 255 characters")
        private String label;

        @Pattern(regexp = "^(text|number|mixed)$", message = "Custom field type must be one of: text, number, mixed")
        private String type;

        @Size(max = 1000, message = "Custom field value cannot exceed 1000 characters")
        private String value;

        public String getLabel() { return label; }
        public void setLabel(String label) { this.label = label; }
        public String getType() { return type; }
        public void setType(String type) { this.type = type; }
        public String getValue() { return value; }
        public void setValue(String value) { this.value = value; }
    }

    public static class AdditionalInfoSaveRequest {

        @Size(max = 100, message = "Starting price for 2 day wedding cannot exceed 100 characters")
        private String startingPriceTwoDayWedding;

        @Size(max = 100, message = "Wedding take per day cannot exceed 100 characters")
        private String weddingPerDay;

        @Size(max = 200, message = "Turnaround time cannot exceed 200 characters")
        private String turnaroundTime;

        @Size(max = 500, message = "Photography style cannot exceed 500 characters")
        private String photographyStyle;

        @Pattern(regexp = "^(yes|no)?$", message = "Travel & accommodation must be 'yes', 'no', or empty")
        private String travelAccommodationSeparate;

        @Size(max = 1024, message = "Vendor contract URL cannot exceed 1024 characters")
        private String vendorContractUrl;

        @Size(max = 1024, message = "Vendor logo URL cannot exceed 1024 characters")
        private String vendorLogoUrl;

        @Valid
        private List<CustomFieldRequest> customFields = new ArrayList<>();

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
        public List<CustomFieldRequest> getCustomFields() { return customFields; }
        public void setCustomFields(List<CustomFieldRequest> customFields) { this.customFields = customFields; }
    }
}

