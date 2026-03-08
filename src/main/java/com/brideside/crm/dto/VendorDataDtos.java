package com.brideside.crm.dto;

import jakarta.validation.constraints.Size;

import java.time.LocalDateTime;

public final class VendorDataDtos {
    private VendorDataDtos() {
    }

    public static class VendorDataResponse {
        private Long id;
        private Long vendorId;
        private String masterDataLink;
        private String calendarSheetLink;
        private LocalDateTime createdAt;
        private LocalDateTime updatedAt;

        public Long getId() { return id; }
        public void setId(Long id) { this.id = id; }
        public Long getVendorId() { return vendorId; }
        public void setVendorId(Long vendorId) { this.vendorId = vendorId; }
        public String getMasterDataLink() { return masterDataLink; }
        public void setMasterDataLink(String masterDataLink) { this.masterDataLink = masterDataLink; }
        public String getCalendarSheetLink() { return calendarSheetLink; }
        public void setCalendarSheetLink(String calendarSheetLink) { this.calendarSheetLink = calendarSheetLink; }
        public LocalDateTime getCreatedAt() { return createdAt; }
        public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }
        public LocalDateTime getUpdatedAt() { return updatedAt; }
        public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
    }

    public static class VendorDataCreateRequest {
        @Size(max = 65535, message = "Master data link cannot exceed maximum length")
        private String masterDataLink;

        @Size(max = 65535, message = "Calendar sheet link cannot exceed maximum length")
        private String calendarSheetLink;

        public String getMasterDataLink() { return masterDataLink; }
        public void setMasterDataLink(String masterDataLink) { this.masterDataLink = masterDataLink; }
        public String getCalendarSheetLink() { return calendarSheetLink; }
        public void setCalendarSheetLink(String calendarSheetLink) { this.calendarSheetLink = calendarSheetLink; }
    }

    public static class VendorDataUpdateRequest {
        @Size(max = 65535, message = "Master data link cannot exceed maximum length")
        private String masterDataLink;

        @Size(max = 65535, message = "Calendar sheet link cannot exceed maximum length")
        private String calendarSheetLink;

        public String getMasterDataLink() { return masterDataLink; }
        public void setMasterDataLink(String masterDataLink) { this.masterDataLink = masterDataLink; }
        public String getCalendarSheetLink() { return calendarSheetLink; }
        public void setCalendarSheetLink(String calendarSheetLink) { this.calendarSheetLink = calendarSheetLink; }
    }
}
