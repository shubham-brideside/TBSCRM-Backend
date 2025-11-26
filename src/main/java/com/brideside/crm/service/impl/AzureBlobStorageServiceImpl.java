package com.brideside.crm.service.impl;

import com.azure.core.util.BinaryData;
import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.brideside.crm.config.AzureBlobStorageProperties;
import com.brideside.crm.service.AzureBlobStorageService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.io.InputStream;
import java.time.Instant;
import java.util.UUID;

@Service
@ConditionalOnExpression("'${azure.storage.blob.connection-string:}' != ''")
public class AzureBlobStorageServiceImpl implements AzureBlobStorageService {

    private static final Logger log = LoggerFactory.getLogger(AzureBlobStorageServiceImpl.class);
    
    private final AzureBlobStorageProperties properties;
    private final BlobServiceClient blobServiceClient;
    private final BlobContainerClient containerClient;

    @Autowired
    public AzureBlobStorageServiceImpl(AzureBlobStorageProperties properties) {
        this.properties = properties;
        
        String connectionString = properties.getConnectionString();
        
        if (!StringUtils.hasText(connectionString)) {
            throw new IllegalStateException("Azure Blob Storage connection string is not configured. Set AZURE_STORAGE_BLOB_CONNECTION_STRING environment variable.");
        }
        
        // Trim and clean the connection string (remove any surrounding quotes)
        connectionString = connectionString.trim();
        if (connectionString.startsWith("\"") && connectionString.endsWith("\"")) {
            connectionString = connectionString.substring(1, connectionString.length() - 1).trim();
        }
        if (connectionString.startsWith("'") && connectionString.endsWith("'")) {
            connectionString = connectionString.substring(1, connectionString.length() - 1).trim();
        }
        
        // Validate connection string format
        if (!connectionString.startsWith("DefaultEndpointsProtocol=")) {
            log.error("Invalid Azure Blob Storage connection string format. Connection string should start with 'DefaultEndpointsProtocol='. Received (first 50 chars): {}", 
                      connectionString.length() > 50 ? connectionString.substring(0, 50) : connectionString);
            throw new IllegalStateException("Invalid Azure Blob Storage connection string format. Connection string should start with 'DefaultEndpointsProtocol='. Please verify the AZURE_STORAGE_BLOB_CONNECTION_STRING environment variable.");
        }
        
        log.info("Initializing Azure Blob Storage with connection string (first 50 chars): {}...", 
                 connectionString.length() > 50 ? connectionString.substring(0, 50) : connectionString);
        
        try {
            // Initialize Azure Blob Storage client
            this.blobServiceClient = new BlobServiceClientBuilder()
                    .connectionString(connectionString)
                    .buildClient();
        } catch (IllegalArgumentException e) {
            log.error("Invalid Azure Blob Storage connection string. Please check the format. Error: {}", e.getMessage());
            log.error("Connection string length: {}, starts with: {}", connectionString.length(), 
                     connectionString.length() > 20 ? connectionString.substring(0, 20) : connectionString);
            throw new IllegalStateException("Invalid Azure Blob Storage connection string. Please verify the AZURE_STORAGE_BLOB_CONNECTION_STRING environment variable is set correctly. Error: " + e.getMessage(), e);
        }
        
        // Get or create container
        this.containerClient = blobServiceClient.getBlobContainerClient(properties.getContainerName());
        
        // Create container if it doesn't exist
        if (!containerClient.exists()) {
            log.info("Creating Azure Blob Storage container: {}", properties.getContainerName());
            containerClient.create();
        }
        
        log.info("Azure Blob Storage service initialized. Container: {}", properties.getContainerName());
    }

    @Override
    public String uploadImage(InputStream fileInputStream, String fileName, String contentType) throws Exception {
        try {
            // Generate unique blob name: {timestamp}-{uuid}-{originalFileName}
            String timestamp = String.valueOf(Instant.now().toEpochMilli());
            String uniqueId = UUID.randomUUID().toString().substring(0, 8);
            String sanitizedFileName = sanitizeFileName(fileName);
            String blobName = String.format("%s-%s-%s", timestamp, uniqueId, sanitizedFileName);
            
            log.debug("Uploading image to Azure Blob Storage: {}", blobName);
            
            // Get blob client
            BlobClient blobClient = containerClient.getBlobClient(blobName);
            
            // Upload file
            blobClient.upload(BinaryData.fromStream(fileInputStream), true);
            
            // Set content type
            com.azure.storage.blob.models.BlobHttpHeaders headers = new com.azure.storage.blob.models.BlobHttpHeaders();
            headers.setContentType(contentType);
            blobClient.setHttpHeaders(headers);
            
            // Get the URL
            String blobUrl = blobClient.getBlobUrl();
            
            log.info("Successfully uploaded image to Azure Blob Storage: {}", blobUrl);
            
            return blobUrl;
        } catch (Exception e) {
            log.error("Failed to upload image to Azure Blob Storage: {}", e.getMessage(), e);
            throw new Exception("Failed to upload image to Azure Blob Storage: " + e.getMessage(), e);
        }
    }

    @Override
    public void deleteFile(String blobUrl) throws Exception {
        try {
            // Extract blob name from URL
            String blobName = extractBlobNameFromUrl(blobUrl);
            
            if (blobName == null) {
                log.warn("Could not extract blob name from URL: {}", blobUrl);
                return;
            }
            
            log.debug("Deleting blob from Azure Blob Storage: {}", blobName);
            
            BlobClient blobClient = containerClient.getBlobClient(blobName);
            
            if (blobClient.exists()) {
                blobClient.delete();
                log.info("Successfully deleted blob from Azure Blob Storage: {}", blobName);
            } else {
                log.warn("Blob does not exist, skipping deletion: {}", blobName);
            }
        } catch (Exception e) {
            log.error("Failed to delete blob from Azure Blob Storage: {}", e.getMessage(), e);
            throw new Exception("Failed to delete blob from Azure Blob Storage: " + e.getMessage(), e);
        }
    }

    private String sanitizeFileName(String fileName) {
        if (fileName == null) {
            return "image";
        }
        // Remove path separators and special characters, keep only alphanumeric, dots, hyphens, underscores
        return fileName.replaceAll("[^a-zA-Z0-9._-]", "_");
    }

    private String extractBlobNameFromUrl(String blobUrl) {
        if (blobUrl == null || !blobUrl.contains("/")) {
            return null;
        }
        try {
            // Extract blob name from URL like: https://account.blob.core.windows.net/container/blob-name
            String[] parts = blobUrl.split("/");
            if (parts.length >= 4) {
                // Find the container name index and get everything after it
                for (int i = 0; i < parts.length; i++) {
                    if (parts[i].equals(properties.getContainerName()) && i + 1 < parts.length) {
                        // Reconstruct blob name from remaining parts
                        StringBuilder blobName = new StringBuilder();
                        for (int j = i + 1; j < parts.length; j++) {
                            if (blobName.length() > 0) {
                                blobName.append("/");
                            }
                            blobName.append(parts[j]);
                        }
                        return blobName.toString();
                    }
                }
            }
        } catch (Exception e) {
            log.warn("Error extracting blob name from URL: {}", e.getMessage());
        }
        return null;
    }
}

