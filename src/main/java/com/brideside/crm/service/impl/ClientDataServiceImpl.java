package com.brideside.crm.service.impl;

import com.brideside.crm.dto.ClientDataDtos;
import com.brideside.crm.entity.ClientData;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.ClientDataRepository;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.service.AzureBlobStorageService;
import com.brideside.crm.service.ClientDataService;
import com.brideside.crm.service.OrganizationProgressService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

@Service
@Transactional
public class ClientDataServiceImpl implements ClientDataService {

    private static final String CONTENT_TYPE_PDF = "application/pdf";
    private static final long MAX_FILE_SIZE = 10 * 1024 * 1024; // 10MB
    private static final String BLOB_PATH_PREFIX = "client-data/";

    private final ClientDataRepository clientDataRepository;
    private final OrganizationRepository organizationRepository;
    private final OrganizationProgressService organizationProgressService;

    @Autowired(required = false)
    private AzureBlobStorageService azureBlobStorageService;

    public ClientDataServiceImpl(ClientDataRepository clientDataRepository,
                                 OrganizationRepository organizationRepository,
                                 OrganizationProgressService organizationProgressService) {
        this.clientDataRepository = clientDataRepository;
        this.organizationRepository = organizationRepository;
        this.organizationProgressService = organizationProgressService;
    }

    @Override
    @Transactional(readOnly = true)
    public ClientDataDtos.ClientDataResponse getByOrganization(Long organizationId) {
        validateOrganization(organizationId);
        return clientDataRepository.findByOrganization_Id(organizationId)
                .map(this::toResponse)
                .orElse(null);
    }

    @Override
    public ClientDataDtos.ClientDataResponse uploadQuoteFormat(Long organizationId, MultipartFile file) {
        validateOrganization(organizationId);
        validatePdfFile(file);
        String blobUrl = uploadToAzure(file, "quote-format");
        ClientData data = getOrCreateClientData(organizationId);
        if (data.getQuoteFormatUrl() != null && azureBlobStorageService != null) {
            try {
                azureBlobStorageService.deleteFile(data.getQuoteFormatUrl());
            } catch (Exception e) {
                // Log but don't fail - old blob may already be deleted
            }
        }
        data.setQuoteFormatUrl(blobUrl);
        ClientData saved = clientDataRepository.save(data);
        organizationProgressService.recomputeAndPersistProgress(organizationId);
        return toResponse(saved);
    }

    @Override
    public ClientDataDtos.ClientDataResponse uploadClientContractFormat(Long organizationId, MultipartFile file) {
        validateOrganization(organizationId);
        validatePdfFile(file);
        String blobUrl = uploadToAzure(file, "client-contract");
        ClientData data = getOrCreateClientData(organizationId);
        if (data.getClientContractFormatUrl() != null && azureBlobStorageService != null) {
            try {
                azureBlobStorageService.deleteFile(data.getClientContractFormatUrl());
            } catch (Exception e) {
                // Log but don't fail
            }
        }
        data.setClientContractFormatUrl(blobUrl);
        ClientData saved = clientDataRepository.save(data);
        organizationProgressService.recomputeAndPersistProgress(organizationId);
        return toResponse(saved);
    }

    @Override
    public ClientDataDtos.ClientDataResponse removeQuoteFormat(Long organizationId) {
        validateOrganization(organizationId);
        ClientData data = clientDataRepository.findByOrganization_Id(organizationId)
                .orElse(null);
        if (data == null || data.getQuoteFormatUrl() == null) {
            return emptyResponse(organizationId);
        }
        if (data.getQuoteFormatUrl() != null && azureBlobStorageService != null) {
            try {
                azureBlobStorageService.deleteFile(data.getQuoteFormatUrl());
            } catch (Exception e) {
                // Log but don't fail
            }
        }
        data.setQuoteFormatUrl(null);
        ClientData saved = clientDataRepository.save(data);
        organizationProgressService.recomputeAndPersistProgress(organizationId);
        return toResponse(saved);
    }

    @Override
    public ClientDataDtos.ClientDataResponse removeClientContractFormat(Long organizationId) {
        validateOrganization(organizationId);
        ClientData data = clientDataRepository.findByOrganization_Id(organizationId)
                .orElse(null);
        if (data == null || data.getClientContractFormatUrl() == null) {
            return emptyResponse(organizationId);
        }
        if (data.getClientContractFormatUrl() != null && azureBlobStorageService != null) {
            try {
                azureBlobStorageService.deleteFile(data.getClientContractFormatUrl());
            } catch (Exception e) {
                // Log but don't fail
            }
        }
        data.setClientContractFormatUrl(null);
        ClientData saved = clientDataRepository.save(data);
        organizationProgressService.recomputeAndPersistProgress(organizationId);
        return toResponse(saved);
    }

    @Override
    public void delete(Long organizationId) {
        validateOrganization(organizationId);
        clientDataRepository.findByOrganization_Id(organizationId).ifPresent(data -> {
            if (azureBlobStorageService != null) {
                try {
                    if (data.getQuoteFormatUrl() != null) azureBlobStorageService.deleteFile(data.getQuoteFormatUrl());
                    if (data.getClientContractFormatUrl() != null) azureBlobStorageService.deleteFile(data.getClientContractFormatUrl());
                } catch (Exception e) {
                    // Log but don't fail
                }
            }
            clientDataRepository.delete(data);
            organizationProgressService.recomputeAndPersistProgress(organizationId);
        });
    }

    private Organization validateOrganization(Long organizationId) {
        if (organizationId == null) {
            throw new BadRequestException("Organization id is required");
        }
        return organizationRepository.findById(organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Organization not found with id " + organizationId));
    }

    private void validatePdfFile(MultipartFile file) {
        if (file == null || file.isEmpty()) {
            throw new BadRequestException("PDF file is required");
        }
        String contentType = file.getContentType();
        if (contentType == null || !contentType.equals(CONTENT_TYPE_PDF)) {
            throw new BadRequestException("Only PDF files are allowed. Received: " + contentType);
        }
        if (file.getSize() > MAX_FILE_SIZE) {
            throw new BadRequestException("File size exceeds maximum allowed size of 10MB");
        }
    }

    private String uploadToAzure(MultipartFile file, String fileType) {
        if (azureBlobStorageService == null) {
            throw new IllegalStateException("Azure Blob Storage is not configured. Set AZURE_STORAGE_BLOB_CONNECTION_STRING.");
        }
        try {
            String fileName = file.getOriginalFilename() != null ? file.getOriginalFilename() : fileType + ".pdf";
            if (!fileName.toLowerCase().endsWith(".pdf")) {
                fileName = fileName + ".pdf";
            }
            return azureBlobStorageService.uploadFile(
                    file.getInputStream(),
                    fileName,
                    CONTENT_TYPE_PDF,
                    BLOB_PATH_PREFIX
            );
        } catch (Exception e) {
            throw new RuntimeException("Failed to upload PDF to Azure: " + e.getMessage(), e);
        }
    }

    private ClientData getOrCreateClientData(Long organizationId) {
        Organization org = organizationRepository.findById(organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Organization not found with id " + organizationId));
        return clientDataRepository.findByOrganization_Id(organizationId)
                .orElseGet(() -> {
                    ClientData data = new ClientData();
                    data.setOrganization(org);
                    return clientDataRepository.save(data);
                });
    }

    private ClientDataDtos.ClientDataResponse toResponse(ClientData data) {
        ClientDataDtos.ClientDataResponse response = new ClientDataDtos.ClientDataResponse();
        response.setId(data.getId());
        response.setOrganizationId(data.getOrganization() != null ? data.getOrganization().getId() : null);
        response.setQuoteFormatUrl(data.getQuoteFormatUrl());
        response.setClientContractFormatUrl(data.getClientContractFormatUrl());
        response.setCreatedAt(data.getCreatedAt());
        response.setUpdatedAt(data.getUpdatedAt());
        return response;
    }

    private ClientDataDtos.ClientDataResponse emptyResponse(Long organizationId) {
        ClientDataDtos.ClientDataResponse response = new ClientDataDtos.ClientDataResponse();
        response.setOrganizationId(organizationId);
        response.setQuoteFormatUrl(null);
        response.setClientContractFormatUrl(null);
        return response;
    }
}
