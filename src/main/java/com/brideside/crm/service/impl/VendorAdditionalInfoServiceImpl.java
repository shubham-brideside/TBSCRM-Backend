package com.brideside.crm.service.impl;

import com.brideside.crm.dto.VendorAdditionalInfoDtos;
import com.brideside.crm.entity.BridesideVendor;
import com.brideside.crm.entity.VendorAdditionalCustomField;
import com.brideside.crm.entity.VendorAdditionalInfo;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.BridesideVendorRepository;
import com.brideside.crm.repository.VendorAdditionalInfoRepository;
import com.brideside.crm.service.AzureBlobStorageService;
import com.brideside.crm.service.OrganizationProgressService;
import com.brideside.crm.service.VendorAdditionalInfoService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Transactional
public class VendorAdditionalInfoServiceImpl implements VendorAdditionalInfoService {

    private static final String CONTENT_TYPE_PDF = "application/pdf";
    private static final long MAX_FILE_SIZE = 10 * 1024 * 1024; // 10MB
    private static final String BLOB_PATH_PREFIX = "vendor-additional-info/";

    /** Allowed for vendor contract and vendor logo: PDF and common image formats. */
    private static final Set<String> ALLOWED_UPLOAD_CONTENT_TYPES = Set.of(
            "application/pdf",
            "image/jpeg",
            "image/jpg",
            "image/png",
            "image/webp",
            "image/gif"
    );

    private final VendorAdditionalInfoRepository vendorAdditionalInfoRepository;
    private final BridesideVendorRepository bridesideVendorRepository;
    private final OrganizationProgressService organizationProgressService;

    @Autowired(required = false)
    private AzureBlobStorageService azureBlobStorageService;

    public VendorAdditionalInfoServiceImpl(VendorAdditionalInfoRepository vendorAdditionalInfoRepository,
                                           BridesideVendorRepository bridesideVendorRepository,
                                           OrganizationProgressService organizationProgressService) {
        this.vendorAdditionalInfoRepository = vendorAdditionalInfoRepository;
        this.bridesideVendorRepository = bridesideVendorRepository;
        this.organizationProgressService = organizationProgressService;
    }

    @Override
    @Transactional(readOnly = true)
    public VendorAdditionalInfoDtos.AdditionalInfoResponse getByVendor(Long organizationId, Long vendorId) {
        BridesideVendor vendor = validateOrgAndVendor(organizationId, vendorId);
        return vendorAdditionalInfoRepository.findByVendor_Id(vendor.getId())
                .map(this::toResponse)
                .orElse(null);
    }

    @Override
    public VendorAdditionalInfoDtos.AdditionalInfoResponse saveForVendor(Long organizationId, Long vendorId,
                                                                         VendorAdditionalInfoDtos.AdditionalInfoSaveRequest request) {
        BridesideVendor vendor = validateOrgAndVendor(organizationId, vendorId);

        VendorAdditionalInfo info = vendorAdditionalInfoRepository.findByVendor_Id(vendorId)
                .orElseGet(() -> {
                    VendorAdditionalInfo newInfo = new VendorAdditionalInfo();
                    newInfo.setVendor(vendor);
                    return newInfo;
                });

        if (request == null) {
            request = new VendorAdditionalInfoDtos.AdditionalInfoSaveRequest();
        }

        info.setStartingPriceTwoDayWedding(trimmed(request.getStartingPriceTwoDayWedding()));
        info.setWeddingPerDay(trimmed(request.getWeddingPerDay()));
        info.setTurnaroundTime(trimmed(request.getTurnaroundTime()));
        info.setPhotographyStyle(trimmed(request.getPhotographyStyle()));

        String travel = trimmed(request.getTravelAccommodationSeparate());
        if (travel != null && !travel.isEmpty()) {
            String normalized = travel.toLowerCase(Locale.ROOT);
            if (!normalized.equals("yes") && !normalized.equals("no")) {
                throw new BadRequestException("Travel & accommodation must be 'yes', 'no', or empty");
            }
            info.setTravelAccommodationSeparate(normalized);
        } else {
            info.setTravelAccommodationSeparate(null);
        }

        info.setVendorContractUrl(trimmed(request.getVendorContractUrl()));
        info.setVendorLogoUrl(trimmed(request.getVendorLogoUrl()));

        List<VendorAdditionalInfoDtos.CustomFieldRequest> fieldRequests =
                request.getCustomFields() != null ? request.getCustomFields() : new ArrayList<>();

        info.getCustomFields().clear();
        for (VendorAdditionalInfoDtos.CustomFieldRequest fieldRequest : fieldRequests) {
            if (fieldRequest.getLabel() == null || fieldRequest.getLabel().trim().isEmpty()) {
                continue;
            }
            VendorAdditionalCustomField field = new VendorAdditionalCustomField();
            field.setAdditionalInfo(info);
            field.setLabel(trimmed(fieldRequest.getLabel()));
            field.setType(trimmed(fieldRequest.getType()));
            field.setValue(trimmed(fieldRequest.getValue()));
            info.getCustomFields().add(field);
        }

        VendorAdditionalInfo saved = vendorAdditionalInfoRepository.save(info);
        organizationProgressService.recomputeAndPersistProgress(organizationId);
        return toResponse(saved);
    }

    @Override
    public VendorAdditionalInfoDtos.AdditionalInfoResponse uploadVendorContract(Long organizationId, Long vendorId,
                                                                                MultipartFile file) {
        validateOrgAndVendor(organizationId, vendorId);
        String contentType = validateDocumentOrImageFile(file);
        String blobUrl = uploadToAzure(file, "vendor-contract-" + vendorId, contentType);

        VendorAdditionalInfo info = vendorAdditionalInfoRepository.findByVendor_Id(vendorId)
                .orElseGet(() -> {
                    VendorAdditionalInfo newInfo = new VendorAdditionalInfo();
                    BridesideVendor vendor = bridesideVendorRepository.findById(vendorId)
                            .orElseThrow(() -> new ResourceNotFoundException("Vendor not found with id " + vendorId));
                    newInfo.setVendor(vendor);
                    return newInfo;
                });

        if (info.getVendorContractUrl() != null && azureBlobStorageService != null) {
            try {
                azureBlobStorageService.deleteFile(info.getVendorContractUrl());
            } catch (Exception e) {
                // ignore old blob delete failures
            }
        }

        info.setVendorContractUrl(blobUrl);
        VendorAdditionalInfo saved = vendorAdditionalInfoRepository.save(info);
        organizationProgressService.recomputeAndPersistProgress(organizationId);
        return toResponse(saved);
    }

    @Override
    public VendorAdditionalInfoDtos.AdditionalInfoResponse uploadVendorLogo(Long organizationId, Long vendorId,
                                                                            MultipartFile file) {
        validateOrgAndVendor(organizationId, vendorId);
        String contentType = validateDocumentOrImageFile(file);
        String blobUrl = uploadToAzure(file, "vendor-logo-" + vendorId, contentType);

        VendorAdditionalInfo info = vendorAdditionalInfoRepository.findByVendor_Id(vendorId)
                .orElseGet(() -> {
                    VendorAdditionalInfo newInfo = new VendorAdditionalInfo();
                    BridesideVendor vendor = bridesideVendorRepository.findById(vendorId)
                            .orElseThrow(() -> new ResourceNotFoundException("Vendor not found with id " + vendorId));
                    newInfo.setVendor(vendor);
                    return newInfo;
                });

        if (info.getVendorLogoUrl() != null && azureBlobStorageService != null) {
            try {
                azureBlobStorageService.deleteFile(info.getVendorLogoUrl());
            } catch (Exception e) {
                // ignore old blob delete failures
            }
        }

        info.setVendorLogoUrl(blobUrl);
        VendorAdditionalInfo saved = vendorAdditionalInfoRepository.save(info);
        organizationProgressService.recomputeAndPersistProgress(organizationId);
        return toResponse(saved);
    }

    private BridesideVendor validateOrgAndVendor(Long organizationId, Long vendorId) {
        if (organizationId == null || vendorId == null) {
            throw new BadRequestException("Organization id and vendor id are required");
        }
        return bridesideVendorRepository.findByIdAndOrganization_Id(vendorId, organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Vendor not found with id " + vendorId + " for organization " + organizationId));
    }

    private VendorAdditionalInfoDtos.AdditionalInfoResponse toResponse(VendorAdditionalInfo info) {
        VendorAdditionalInfoDtos.AdditionalInfoResponse response = new VendorAdditionalInfoDtos.AdditionalInfoResponse();
        response.setId(info.getId());
        response.setVendorId(info.getVendor() != null ? info.getVendor().getId() : null);
        response.setStartingPriceTwoDayWedding(info.getStartingPriceTwoDayWedding());
        response.setWeddingPerDay(info.getWeddingPerDay());
        response.setTurnaroundTime(info.getTurnaroundTime());
        response.setPhotographyStyle(info.getPhotographyStyle());
        response.setTravelAccommodationSeparate(info.getTravelAccommodationSeparate());
        response.setVendorContractUrl(info.getVendorContractUrl());
        response.setVendorLogoUrl(info.getVendorLogoUrl());
        response.setCreatedAt(info.getCreatedAt());
        response.setUpdatedAt(info.getUpdatedAt());

        List<VendorAdditionalInfoDtos.CustomFieldResponse> customFieldResponses = info.getCustomFields()
                .stream()
                .map(field -> {
                    VendorAdditionalInfoDtos.CustomFieldResponse dto = new VendorAdditionalInfoDtos.CustomFieldResponse();
                    dto.setId(field.getId());
                    dto.setLabel(field.getLabel());
                    dto.setType(field.getType());
                    dto.setValue(field.getValue());
                    dto.setCreatedAt(field.getCreatedAt());
                    dto.setUpdatedAt(field.getUpdatedAt());
                    return dto;
                })
                .collect(Collectors.toList());
        response.setCustomFields(customFieldResponses);
        return response;
    }

    /**
     * Validates file is present, within size limit, and has an allowed content type (PDF or image).
     * Returns the validated content type for use when uploading to Azure.
     */
    private String validateDocumentOrImageFile(MultipartFile file) {
        if (file == null || file.isEmpty()) {
            throw new BadRequestException("File is required");
        }
        String contentType = file.getContentType();
        if (contentType == null || contentType.isBlank()) {
            throw new BadRequestException("File content type is required");
        }
        String normalized = contentType.toLowerCase(Locale.ROOT).trim();
        if (!ALLOWED_UPLOAD_CONTENT_TYPES.contains(normalized)) {
            throw new BadRequestException(
                    "Allowed formats: PDF, JPEG, JPG, PNG, WebP, GIF. Received: " + contentType);
        }
        if (file.getSize() > MAX_FILE_SIZE) {
            throw new BadRequestException("File size exceeds maximum allowed size of 10MB");
        }
        return normalized;
    }

    private String uploadToAzure(MultipartFile file, String baseFileName, String contentType) {
        if (azureBlobStorageService == null) {
            throw new IllegalStateException("Azure Blob Storage is not configured. Set AZURE_STORAGE_BLOB_CONNECTION_STRING.");
        }
        try {
            String originalName = file.getOriginalFilename();
            String extension = "";
            if (originalName != null && originalName.contains(".")) {
                extension = originalName.substring(originalName.lastIndexOf('.')).toLowerCase(Locale.ROOT);
            } else {
                extension = extensionFromContentType(contentType);
            }
            String fileName = baseFileName + extension;
            return azureBlobStorageService.uploadFile(
                    file.getInputStream(),
                    fileName,
                    contentType,
                    BLOB_PATH_PREFIX
            );
        } catch (Exception e) {
            throw new RuntimeException("Failed to upload file to Azure: " + e.getMessage(), e);
        }
    }

    private static String extensionFromContentType(String contentType) {
        if (contentType == null) return "";
        return switch (contentType) {
            case "application/pdf" -> ".pdf";
            case "image/jpeg", "image/jpg" -> ".jpg";
            case "image/png" -> ".png";
            case "image/webp" -> ".webp";
            case "image/gif" -> ".gif";
            default -> ".bin";
        };
    }

    private String trimmed(String value) {
        return value == null ? null : value.trim();
    }
}

