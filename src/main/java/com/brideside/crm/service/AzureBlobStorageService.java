package com.brideside.crm.service;

import java.io.InputStream;

/**
 * Service for uploading files to Azure Blob Storage
 */
public interface AzureBlobStorageService {
    /**
     * Upload an image file to Azure Blob Storage
     * @param fileInputStream The file input stream
     * @param fileName The file name (will be prefixed with activity ID and timestamp)
     * @param contentType The content type (e.g., "image/png", "image/jpeg")
     * @return The public URL of the uploaded file
     * @throws Exception if upload fails
     */
    String uploadImage(InputStream fileInputStream, String fileName, String contentType) throws Exception;
    
    /**
     * Delete a file from Azure Blob Storage
     * @param blobUrl The URL of the blob to delete
     * @throws Exception if deletion fails
     */
    void deleteFile(String blobUrl) throws Exception;
}

