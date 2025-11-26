package com.brideside.crm.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties(prefix = "azure.storage.blob")
public class AzureBlobStorageProperties {

    /**
     * Azure Storage Account connection string
     * Format: DefaultEndpointsProtocol=https;AccountName=...;AccountKey=...;EndpointSuffix=core.windows.net
     */
    private String connectionString;

    /**
     * Container name where activity screenshots will be stored
     */
    private String containerName = "call-screenshots";

    /**
     * Base URL for accessing uploaded files (optional, will be constructed from connection string if not provided)
     */
    private String baseUrl;

    public String getConnectionString() {
        return connectionString;
    }

    public void setConnectionString(String connectionString) {
        this.connectionString = connectionString;
    }

    public String getContainerName() {
        return containerName;
    }

    public void setContainerName(String containerName) {
        this.containerName = containerName;
    }

    public String getBaseUrl() {
        return baseUrl;
    }

    public void setBaseUrl(String baseUrl) {
        this.baseUrl = baseUrl;
    }
}

