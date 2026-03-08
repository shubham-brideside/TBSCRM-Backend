package com.brideside.crm.dto;

import java.time.LocalDateTime;

public final class ClientDataDtos {
    private ClientDataDtos() {
    }

    public static class ClientDataResponse {
        private Long id;
        private Long organizationId;
        private String quoteFormatUrl;
        private String clientContractFormatUrl;
        private LocalDateTime createdAt;
        private LocalDateTime updatedAt;

        public Long getId() { return id; }
        public void setId(Long id) { this.id = id; }
        public Long getOrganizationId() { return organizationId; }
        public void setOrganizationId(Long organizationId) { this.organizationId = organizationId; }
        public String getQuoteFormatUrl() { return quoteFormatUrl; }
        public void setQuoteFormatUrl(String quoteFormatUrl) { this.quoteFormatUrl = quoteFormatUrl; }
        public String getClientContractFormatUrl() { return clientContractFormatUrl; }
        public void setClientContractFormatUrl(String clientContractFormatUrl) { this.clientContractFormatUrl = clientContractFormatUrl; }
        public LocalDateTime getCreatedAt() { return createdAt; }
        public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }
        public LocalDateTime getUpdatedAt() { return updatedAt; }
        public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }
    }
}
