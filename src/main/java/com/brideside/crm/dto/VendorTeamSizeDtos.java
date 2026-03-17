package com.brideside.crm.dto;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

public final class VendorTeamSizeDtos {
    private VendorTeamSizeDtos() {
    }

    public static class TeamSizeRowResponse {
        private Long id;
        private Long vendorId;
        private String guestCount;
        private String eventType;
        private String photographer;
        private String cinematographer;
        private String drone;
        private String notes;
        private LocalDateTime createdAt;
        private LocalDateTime updatedAt;

        public Long getId() { return id; }
        public void setId(Long id) { this.id = id; }
        public Long getVendorId() { return vendorId; }
        public void setVendorId(Long vendorId) { this.vendorId = vendorId; }
        public String getGuestCount() { return guestCount; }
        public void setGuestCount(String guestCount) { this.guestCount = guestCount; }
        public String getEventType() { return eventType; }
        public void setEventType(String eventType) { this.eventType = eventType; }
        public String getPhotographer() { return photographer; }
        public void setPhotographer(String photographer) { this.photographer = photographer; }
        public String getCinematographer() { return cinematographer; }
        public void setCinematographer(String cinematographer) { this.cinematographer = cinematographer; }
        public String getDrone() { return drone; }
        public void setDrone(String drone) { this.drone = drone; }
        public String getNotes() { return notes; }
        public void setNotes(String notes) { this.notes = notes; }
        public LocalDateTime getCreatedAt() { return createdAt; }
        public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }
        public LocalDateTime getUpdatedAt() { return updatedAt; }
        public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
    }

    public static class TeamSizeRowRequest {
        private Long id;

        @NotBlank(message = "Guest count is required")
        @Size(max = 100, message = "Guest count cannot exceed 100 characters")
        private String guestCount;

        @NotBlank(message = "Event type is required")
        @Size(max = 100, message = "Event type cannot exceed 100 characters")
        private String eventType;

        @Size(max = 500, message = "Photographer cannot exceed 500 characters")
        private String photographer;

        @Size(max = 500, message = "Cinematographer cannot exceed 500 characters")
        private String cinematographer;

        @Size(max = 500, message = "Drone cannot exceed 500 characters")
        private String drone;

        @Size(max = 1000, message = "Notes cannot exceed 1000 characters")
        private String notes;

        public Long getId() { return id; }
        public void setId(Long id) { this.id = id; }
        public String getGuestCount() { return guestCount; }
        public void setGuestCount(String guestCount) { this.guestCount = guestCount; }
        public String getEventType() { return eventType; }
        public void setEventType(String eventType) { this.eventType = eventType; }
        public String getPhotographer() { return photographer; }
        public void setPhotographer(String photographer) { this.photographer = photographer; }
        public String getCinematographer() { return cinematographer; }
        public void setCinematographer(String cinematographer) { this.cinematographer = cinematographer; }
        public String getDrone() { return drone; }
        public void setDrone(String drone) { this.drone = drone; }
        public String getNotes() { return notes; }
        public void setNotes(String notes) { this.notes = notes; }
    }

    public static class TeamSizeRowsResponse {
        private List<TeamSizeRowResponse> rows = new ArrayList<>();

        public List<TeamSizeRowResponse> getRows() { return rows; }
        public void setRows(List<TeamSizeRowResponse> rows) { this.rows = rows; }
    }

    public static class TeamSizeSaveRequest {
        @Valid
        private List<TeamSizeRowRequest> rows = new ArrayList<>();

        public List<TeamSizeRowRequest> getRows() { return rows; }
        public void setRows(List<TeamSizeRowRequest> rows) { this.rows = rows; }
    }
}

