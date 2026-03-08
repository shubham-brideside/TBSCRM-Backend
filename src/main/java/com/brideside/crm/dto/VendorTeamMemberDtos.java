package com.brideside.crm.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import java.time.LocalDateTime;

public final class VendorTeamMemberDtos {
    private VendorTeamMemberDtos() {
    }

    public static class TeamMemberResponse {
        private Long id;
        private Long vendorId;
        private String name;
        private String designation;
        private String instagramId;
        private LocalDateTime createdAt;
        private LocalDateTime updatedAt;

        public Long getId() { return id; }
        public void setId(Long id) { this.id = id; }
        public Long getVendorId() { return vendorId; }
        public void setVendorId(Long vendorId) { this.vendorId = vendorId; }
        public String getName() { return name; }
        public void setName(String name) { this.name = name; }
        public String getDesignation() { return designation; }
        public void setDesignation(String designation) { this.designation = designation; }
        public String getInstagramId() { return instagramId; }
        public void setInstagramId(String instagramId) { this.instagramId = instagramId; }
        public LocalDateTime getCreatedAt() { return createdAt; }
        public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }
        public LocalDateTime getUpdatedAt() { return updatedAt; }
        public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
    }

    public static class TeamMemberCreateRequest {
        @NotBlank(message = "Name is required")
        @Size(max = 255, message = "Name cannot exceed 255 characters")
        private String name;

        @Size(max = 255, message = "Designation cannot exceed 255 characters")
        private String designation;

        @Size(max = 255, message = "Instagram ID cannot exceed 255 characters")
        private String instagramId;

        public String getName() { return name; }
        public void setName(String name) { this.name = name; }
        public String getDesignation() { return designation; }
        public void setDesignation(String designation) { this.designation = designation; }
        public String getInstagramId() { return instagramId; }
        public void setInstagramId(String instagramId) { this.instagramId = instagramId; }
    }

    public static class TeamMemberUpdateRequest {
        @NotBlank(message = "Name is required")
        @Size(max = 255, message = "Name cannot exceed 255 characters")
        private String name;

        @Size(max = 255, message = "Designation cannot exceed 255 characters")
        private String designation;

        @Size(max = 255, message = "Instagram ID cannot exceed 255 characters")
        private String instagramId;

        public String getName() { return name; }
        public void setName(String name) { this.name = name; }
        public String getDesignation() { return designation; }
        public void setDesignation(String designation) { this.designation = designation; }
        public String getInstagramId() { return instagramId; }
        public void setInstagramId(String instagramId) { this.instagramId = instagramId; }
    }
}
