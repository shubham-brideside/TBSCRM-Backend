package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.BridesideVendorDtos;
import com.brideside.crm.dto.ClientDataDtos;
import com.brideside.crm.dto.EventPricingDtos;
import com.brideside.crm.dto.OrganizationDtos;
import com.brideside.crm.dto.OrganizationDetailsDtos;
import com.brideside.crm.dto.OrganizationActivationDtos;
import com.brideside.crm.dto.OrganizationProgressDtos;
import com.brideside.crm.dto.VendorAssetDtos;
import com.brideside.crm.dto.VendorDataDtos;
import com.brideside.crm.dto.VendorTeamMemberDtos;
import com.brideside.crm.dto.VendorTeamSizeDtos;
import com.brideside.crm.dto.VendorAdditionalInfoDtos;
import com.brideside.crm.service.BridesideVendorService;
import com.brideside.crm.service.ClientDataService;
import com.brideside.crm.service.EventPricingService;
import com.brideside.crm.service.OrganizationProgressService;
import com.brideside.crm.service.OrganizationActivationService;
import com.brideside.crm.service.OrganizationService;
import com.brideside.crm.service.VendorAssetService;
import com.brideside.crm.service.VendorDataService;
import com.brideside.crm.service.VendorTeamMemberService;
import com.brideside.crm.service.VendorTeamSizeService;
import com.brideside.crm.service.VendorAdditionalInfoService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.List;

@RestController
@RequestMapping("/api/organizations")
@Tag(name = "Organizations", description = "Organization management APIs")
public class OrganizationController {

    private final OrganizationService organizationService;
    private final OrganizationProgressService organizationProgressService;
    private final OrganizationActivationService organizationActivationService;
    private final BridesideVendorService bridesideVendorService;
    private final VendorAssetService vendorAssetService;
    private final EventPricingService eventPricingService;
    private final VendorTeamMemberService vendorTeamMemberService;
    private final VendorDataService vendorDataService;
    private final ClientDataService clientDataService;
    private final VendorTeamSizeService vendorTeamSizeService;
    private final VendorAdditionalInfoService vendorAdditionalInfoService;
    private final ObjectMapper objectMapper;

    public OrganizationController(OrganizationService organizationService,
                                  OrganizationProgressService organizationProgressService,
                                  OrganizationActivationService organizationActivationService,
                                  BridesideVendorService bridesideVendorService,
                                  VendorAssetService vendorAssetService,
                                  EventPricingService eventPricingService,
                         VendorTeamMemberService vendorTeamMemberService,
                         VendorDataService vendorDataService,
                         ClientDataService clientDataService,
                         VendorTeamSizeService vendorTeamSizeService,
                         VendorAdditionalInfoService vendorAdditionalInfoService,
                         ObjectMapper objectMapper) {
        this.organizationService = organizationService;
        this.organizationProgressService = organizationProgressService;
        this.organizationActivationService = organizationActivationService;
        this.bridesideVendorService = bridesideVendorService;
        this.vendorAssetService = vendorAssetService;
        this.eventPricingService = eventPricingService;
        this.vendorTeamMemberService = vendorTeamMemberService;
        this.vendorDataService = vendorDataService;
        this.clientDataService = clientDataService;
        this.vendorTeamSizeService = vendorTeamSizeService;
        this.vendorAdditionalInfoService = vendorAdditionalInfoService;
        this.objectMapper = objectMapper;
    }

    @PostMapping
    @Operation(summary = "Create organization")
    public ResponseEntity<ApiResponse<OrganizationDtos.OrganizationResponse>> create(
            @Valid @RequestBody OrganizationDtos.OrganizationRequest request) {
        OrganizationDtos.OrganizationResponse response = organizationService.create(request);
        return ResponseEntity.status(201).body(ApiResponse.success("Organization created", response));
    }

    @GetMapping("/categories")
    @Operation(summary = "List organization categories")
    public ResponseEntity<ApiResponse<List<OrganizationDtos.CategoryOption>>> categories() {
        return ResponseEntity.ok(ApiResponse.success("Organization categories fetched", organizationService.listCategoryOptions()));
    }

    @GetMapping("/owners")
    @Operation(summary = "List eligible organization owners")
    public ResponseEntity<ApiResponse<List<OrganizationDtos.OwnerOption>>> ownerOptions() {
        return ResponseEntity.ok(ApiResponse.success("Organization owners fetched", organizationService.listOwnerOptions()));
    }

    @GetMapping
    @Operation(summary = "List organizations")
    public ResponseEntity<ApiResponse<List<OrganizationDtos.OrganizationResponse>>> list() {
        return ResponseEntity.ok(ApiResponse.success("Organizations fetched", organizationService.list()));
    }

    @GetMapping("/accessible-for-current-user")
    @PreAuthorize("isAuthenticated()")
    @Operation(summary = "List organizations accessible to current user", 
               description = "Returns organizations based on role hierarchy. Admin sees all, Category Manager sees their orgs and those of their Sales/Presales, Sales/Presales see their own orgs.")
    @SecurityRequirement(name = "Bearer Authentication")
    public ResponseEntity<ApiResponse<List<OrganizationDtos.OrganizationResponse>>> listAccessibleForCurrentUser() {
        String currentUserEmail = getCurrentUserEmail();
        List<OrganizationDtos.OrganizationResponse> organizations = organizationService.listAccessibleForCurrentUser(currentUserEmail);
        return ResponseEntity.ok(ApiResponse.success("Accessible organizations fetched", organizations));
    }

    @GetMapping("/{id}")
    @Operation(summary = "Get organization by id")
    public ResponseEntity<ApiResponse<OrganizationDtos.OrganizationResponse>> get(@PathVariable("id") Long id) {
        return ResponseEntity.ok(ApiResponse.success("Organization fetched", organizationService.get(id)));
    }

    @GetMapping("/{id:\\d+}/progress")
    @Operation(summary = "Get organization onboarding progress",
            description = "Returns completion status for each section: Organization details, Asset Info, Events Pricing, Vendor Data, Client Data, Team Members. isActive is true when all are complete.")
    public ResponseEntity<ApiResponse<OrganizationProgressDtos.ProgressResponse>> getProgress(@PathVariable("id") Long id) {
        return ResponseEntity.ok(ApiResponse.success("Organization progress fetched", organizationProgressService.getProgress(id)));
    }

    @GetMapping("/{id:\\d+}/activation-checklist")
    @Operation(summary = "Get organization activation checklist",
            description = "Returns activation checklist and progress for the organization. If no record exists yet, returns defaults with all items unchecked.")
    public ResponseEntity<ApiResponse<OrganizationActivationDtos.ProgressResponse>> getActivationChecklist(
            @PathVariable("id") Long id) {
        OrganizationActivationDtos.ProgressResponse response = organizationActivationService.getActivationProgress(id);
        return ResponseEntity.ok(ApiResponse.success("Organization activation checklist fetched", response));
    }

    @PutMapping("/{id:\\d+}/activation-checklist")
    @Operation(summary = "Save organization activation checklist",
            description = "Upserts activation checklist for the organization and recomputes completed/total counts and activated flag.")
    public ResponseEntity<ApiResponse<OrganizationActivationDtos.ProgressResponse>> saveActivationChecklist(
            @PathVariable("id") Long id,
            @RequestBody(required = false) JsonNode body) {
        if (body == null || body.isNull()) {
            body = objectMapper.createObjectNode();
        }
        OrganizationActivationDtos.Checklist checklist = objectMapper.convertValue(body, OrganizationActivationDtos.Checklist.class);
        OrganizationActivationDtos.ProgressResponse response =
                organizationActivationService.saveChecklist(id, checklist, body);
        return ResponseEntity.ok(ApiResponse.success("Organization activation checklist saved", response));
    }

    @GetMapping("/{id:\\d+}/progress/debug")
    @Operation(summary = "Get organization progress (debug)",
            description = "Same as /progress but includes clientDataRecordExists, quoteFormatUrlPresent, clientContractFormatUrlPresent to diagnose clientDataComplete.")
    public ResponseEntity<ApiResponse<OrganizationProgressDtos.ProgressDebugResponse>> getProgressDebug(@PathVariable("id") Long id) {
        return ResponseEntity.ok(ApiResponse.success("Organization progress (debug) fetched", organizationProgressService.getProgressDebug(id)));
    }

    @GetMapping("/{id:\\d+}/with-details")
    @Operation(summary = "Get organization with vendor details",
            description = "Returns organization details along with vendors from brideside_vendors for the given organization id")
    public ResponseEntity<ApiResponse<OrganizationDetailsDtos.OrganizationWithDetailsResponse>> getWithDetails(
            @PathVariable("id") Long id) {
        OrganizationDetailsDtos.OrganizationWithDetailsResponse data = new OrganizationDetailsDtos.OrganizationWithDetailsResponse();
        data.setOrganization(organizationService.get(id));
        data.setVendors(bridesideVendorService.listByOrganizationId(id));
        return ResponseEntity.ok(ApiResponse.success("Organization details fetched", data));
    }

    @GetMapping("/{id}/vendors")
    @Operation(summary = "List Brideside vendors for an organization",
            description = "Returns vendors from brideside_vendors table for the given organization id")
    public ResponseEntity<ApiResponse<List<BridesideVendorDtos.VendorResponse>>> listVendorsForOrganization(
            @PathVariable("id") Long id) {
        return ResponseEntity.ok(ApiResponse.success("Organization vendors fetched", bridesideVendorService.listByOrganizationId(id)));
    }

    @PostMapping("/{id:\\d+}/vendors")
    @Operation(summary = "Create Brideside vendor entry for an organization",
            description = "Creates a brideside_vendors row linked to the organization. If pipelineId is not provided and no pipeline exists for the org, a default pipeline is created.")
    public ResponseEntity<ApiResponse<BridesideVendorDtos.VendorResponse>> createVendorForOrganization(
            @PathVariable("id") Long id,
            @Valid @RequestBody(required = false) BridesideVendorDtos.VendorCreateRequest request) {
        BridesideVendorDtos.VendorResponse created = bridesideVendorService.createVendorForOrganization(id, request);
        return ResponseEntity.status(201).body(ApiResponse.success("Organization vendor created", created));
    }

    @PutMapping("/{id:\\d+}/vendors/{vendorId:\\d+}")
    @Operation(summary = "Update Brideside vendor details for an organization",
            description = "Updates editable vendor fields (contact/location/email/onboarding/team/fee). Does not expose or update access_token.")
    public ResponseEntity<ApiResponse<BridesideVendorDtos.VendorResponse>> updateVendorDetails(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @Valid @RequestBody BridesideVendorDtos.VendorUpdateRequest request) {
        BridesideVendorDtos.VendorResponse updated = bridesideVendorService.updateVendorDetails(id, vendorId, request);
        return ResponseEntity.ok(ApiResponse.success("Organization vendor updated", updated));
    }

    @PatchMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/about")
    @Operation(summary = "Update vendor about details",
            description = "Updates only the about field (details of vendor services). Use this when you have a dedicated About section/editor.")
    public ResponseEntity<ApiResponse<BridesideVendorDtos.VendorResponse>> updateVendorAbout(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @RequestBody(required = false) BridesideVendorDtos.AboutUpdateRequest request) {
        BridesideVendorDtos.VendorResponse updated = bridesideVendorService.updateVendorAbout(id, vendorId, request);
        return ResponseEntity.ok(ApiResponse.success("Vendor about updated", updated));
    }

    @PutMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/event-pricing")
    @Operation(summary = "Save event pricing for a vendor",
            description = "Replaces all event pricing for the vendor. Use eventPricing for Photography (single session, primary only). Use eventPricingCurrent and eventPricingUpcoming for Makeup (with artist levels).")
    public ResponseEntity<ApiResponse<BridesideVendorDtos.VendorResponse>> saveEventPricing(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @RequestBody(required = false) EventPricingDtos.EventPricingUpdateRequest request) {
        eventPricingService.saveEventPricing(id, vendorId, request);
        BridesideVendorDtos.VendorResponse updated = bridesideVendorService.getVendor(id, vendorId);
        return ResponseEntity.ok(ApiResponse.success("Event pricing saved", updated));
    }

    @GetMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/assets")
    @Operation(summary = "List vendor assets for an organization",
            description = "Returns asset info (phone, SIM, etc.) for the given vendor")
    public ResponseEntity<ApiResponse<List<VendorAssetDtos.AssetResponse>>> listVendorAssets(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId) {
        return ResponseEntity.ok(ApiResponse.success("Vendor assets fetched", vendorAssetService.listByVendor(id, vendorId)));
    }

    @PostMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/assets")
    @Operation(summary = "Create vendor asset",
            description = "Creates a new asset (phone, SIM, etc.) for the vendor. Use when no asset exists yet.")
    public ResponseEntity<ApiResponse<VendorAssetDtos.AssetResponse>> createVendorAsset(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @RequestBody(required = false) VendorAssetDtos.AssetUpdateRequest request) {
        VendorAssetDtos.AssetResponse created = vendorAssetService.create(id, vendorId, request != null ? request : new VendorAssetDtos.AssetUpdateRequest());
        return ResponseEntity.status(201).body(ApiResponse.success("Asset created", created));
    }

    @PutMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/assets/{assetId:\\d+}")
    @Operation(summary = "Update vendor asset details",
            description = "Updates phone model, phone issued by, SIM card, SIM issued by, issued on")
    public ResponseEntity<ApiResponse<VendorAssetDtos.AssetResponse>> updateVendorAsset(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @PathVariable("assetId") Long assetId,
            @Valid @RequestBody VendorAssetDtos.AssetUpdateRequest request) {
        VendorAssetDtos.AssetResponse updated = vendorAssetService.update(id, vendorId, assetId, request);
        return ResponseEntity.ok(ApiResponse.success("Vendor asset updated", updated));
    }

    @GetMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/team-members")
    @Operation(summary = "List vendor team members",
            description = "Returns team members (name, designation, instagram) for the given vendor")
    public ResponseEntity<ApiResponse<List<VendorTeamMemberDtos.TeamMemberResponse>>> listVendorTeamMembers(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId) {
        return ResponseEntity.ok(ApiResponse.success("Vendor team members fetched", vendorTeamMemberService.listByVendor(id, vendorId)));
    }

    @PostMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/team-members")
    @Operation(summary = "Create vendor team member",
            description = "Adds a new team member (name, designation, instagram ID) for the vendor")
    public ResponseEntity<ApiResponse<VendorTeamMemberDtos.TeamMemberResponse>> createVendorTeamMember(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @Valid @RequestBody VendorTeamMemberDtos.TeamMemberCreateRequest request) {
        VendorTeamMemberDtos.TeamMemberResponse created = vendorTeamMemberService.create(id, vendorId, request);
        return ResponseEntity.status(201).body(ApiResponse.success("Team member created", created));
    }

    @PutMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/team-members/{memberId:\\d+}")
    @Operation(summary = "Update vendor team member",
            description = "Updates name, designation, and instagram ID for a team member")
    public ResponseEntity<ApiResponse<VendorTeamMemberDtos.TeamMemberResponse>> updateVendorTeamMember(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @PathVariable("memberId") Long memberId,
            @Valid @RequestBody VendorTeamMemberDtos.TeamMemberUpdateRequest request) {
        VendorTeamMemberDtos.TeamMemberResponse updated = vendorTeamMemberService.update(id, vendorId, memberId, request);
        return ResponseEntity.ok(ApiResponse.success("Team member updated", updated));
    }

    @DeleteMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/team-members/{memberId:\\d+}")
    @Operation(summary = "Delete vendor team member",
            description = "Removes a team member from the vendor")
    public ResponseEntity<ApiResponse<Void>> deleteVendorTeamMember(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @PathVariable("memberId") Long memberId) {
        vendorTeamMemberService.delete(id, vendorId, memberId);
        return ResponseEntity.ok(ApiResponse.success("Team member deleted"));
    }

    @GetMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/team-size")
    @Operation(summary = "Get vendor team size rows",
            description = "Returns detailed team size rows for the given vendor (guest count, event type, photographer, cinematographer, drone, notes).")
    public ResponseEntity<ApiResponse<VendorTeamSizeDtos.TeamSizeRowsResponse>> getVendorTeamSize(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId) {
        VendorTeamSizeDtos.TeamSizeRowsResponse response = vendorTeamSizeService.listByVendor(id, vendorId);
        return ResponseEntity.ok(ApiResponse.success("Vendor team size rows fetched", response));
    }

    @PutMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/team-size")
    @Operation(summary = "Save vendor team size rows",
            description = "Replaces all team size rows for the vendor. Send the full list of rows each time.")
    public ResponseEntity<ApiResponse<VendorTeamSizeDtos.TeamSizeRowsResponse>> saveVendorTeamSize(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @Valid @RequestBody(required = false) VendorTeamSizeDtos.TeamSizeSaveRequest request) {
        VendorTeamSizeDtos.TeamSizeRowsResponse saved =
                vendorTeamSizeService.saveForVendor(id, vendorId, request != null ? request : new VendorTeamSizeDtos.TeamSizeSaveRequest());
        return ResponseEntity.ok(ApiResponse.success("Vendor team size rows saved", saved));
    }

    @GetMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/additional-info")
    @Operation(summary = "Get vendor additional info",
            description = "Returns additional info details for the given vendor (pricing, turnaround time, style, travel & accommodation, contract, logo, and custom fields).")
    public ResponseEntity<ApiResponse<VendorAdditionalInfoDtos.AdditionalInfoResponse>> getVendorAdditionalInfo(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId) {
        VendorAdditionalInfoDtos.AdditionalInfoResponse response = vendorAdditionalInfoService.getByVendor(id, vendorId);
        return ResponseEntity.ok(ApiResponse.success("Vendor additional info fetched", response));
    }

    @PutMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/additional-info")
    @Operation(summary = "Save vendor additional info",
            description = "Creates or updates additional info for the vendor, including fixed fields and custom fields.")
    public ResponseEntity<ApiResponse<VendorAdditionalInfoDtos.AdditionalInfoResponse>> saveVendorAdditionalInfo(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @Valid @RequestBody(required = false) VendorAdditionalInfoDtos.AdditionalInfoSaveRequest request) {
        VendorAdditionalInfoDtos.AdditionalInfoResponse saved =
                vendorAdditionalInfoService.saveForVendor(id, vendorId, request != null ? request : new VendorAdditionalInfoDtos.AdditionalInfoSaveRequest());
        return ResponseEntity.ok(ApiResponse.success("Vendor additional info saved", saved));
    }

    @PostMapping(value = "/{id:\\d+}/vendors/{vendorId:\\d+}/additional-info/vendor-contract/upload",
            consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @Operation(summary = "Upload vendor contract",
            description = "Uploads a vendor contract file (PDF or image: JPG, PNG, WebP, GIF) to Azure Blob Storage and saves the public URL in vendor additional info. Replaces existing if any. Max 10MB.")
    public ResponseEntity<ApiResponse<VendorAdditionalInfoDtos.AdditionalInfoResponse>> uploadVendorContract(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @RequestParam("file") MultipartFile file) {
        try {
            VendorAdditionalInfoDtos.AdditionalInfoResponse result =
                    vendorAdditionalInfoService.uploadVendorContract(id, vendorId, file);
            return ResponseEntity.status(201).body(ApiResponse.success("Vendor contract uploaded", result));
        } catch (IllegalStateException e) {
            return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE)
                    .body(ApiResponse.error("Azure Blob Storage is not configured. Set AZURE_STORAGE_BLOB_CONNECTION_STRING."));
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(ApiResponse.error(e.getMessage()));
        }
    }

    @PostMapping(value = "/{id:\\d+}/vendors/{vendorId:\\d+}/additional-info/vendor-logo/upload",
            consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @Operation(summary = "Upload vendor logo",
            description = "Uploads a vendor logo file (PDF or image: JPG, PNG, WebP, GIF) to Azure Blob Storage and saves the public URL in vendor additional info. Replaces existing if any. Max 10MB.")
    public ResponseEntity<ApiResponse<VendorAdditionalInfoDtos.AdditionalInfoResponse>> uploadVendorLogo(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @RequestParam("file") MultipartFile file) {
        try {
            VendorAdditionalInfoDtos.AdditionalInfoResponse result =
                    vendorAdditionalInfoService.uploadVendorLogo(id, vendorId, file);
            return ResponseEntity.status(201).body(ApiResponse.success("Vendor logo uploaded", result));
        } catch (IllegalStateException e) {
            return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE)
                    .body(ApiResponse.error("Azure Blob Storage is not configured. Set AZURE_STORAGE_BLOB_CONNECTION_STRING."));
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(ApiResponse.error(e.getMessage()));
        }
    }

    @GetMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/vendor-data")
    @Operation(summary = "Get vendor data (URLs)",
            description = "Returns master data link and calendar sheet link for the vendor. Returns null if none exists.")
    public ResponseEntity<ApiResponse<VendorDataDtos.VendorDataResponse>> getVendorData(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId) {
        return ResponseEntity.ok(ApiResponse.success("Vendor data fetched", vendorDataService.getByVendor(id, vendorId)));
    }

    @PostMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/vendor-data")
    @Operation(summary = "Create vendor data",
            description = "Creates vendor data (master data link, calendar sheet link). Fails if data already exists for this vendor.")
    public ResponseEntity<ApiResponse<VendorDataDtos.VendorDataResponse>> createVendorData(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @Valid @RequestBody(required = false) VendorDataDtos.VendorDataCreateRequest request) {
        VendorDataDtos.VendorDataResponse created = vendorDataService.create(id, vendorId, request != null ? request : new VendorDataDtos.VendorDataCreateRequest());
        return ResponseEntity.status(201).body(ApiResponse.success("Vendor data created", created));
    }

    @PutMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/vendor-data")
    @Operation(summary = "Save vendor data (upsert)",
            description = "Creates or updates vendor data (master data link, calendar sheet link). Use for the Save button.")
    public ResponseEntity<ApiResponse<VendorDataDtos.VendorDataResponse>> saveVendorData(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @RequestBody(required = false) VendorDataDtos.VendorDataUpdateRequest request) {
        VendorDataDtos.VendorDataResponse saved = vendorDataService.save(id, vendorId, request != null ? request : new VendorDataDtos.VendorDataUpdateRequest());
        return ResponseEntity.ok(ApiResponse.success("Vendor data saved", saved));
    }

    @PutMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/vendor-data/{dataId:\\d+}")
    @Operation(summary = "Update vendor data by id",
            description = "Updates master data link and calendar sheet link for an existing vendor data record.")
    public ResponseEntity<ApiResponse<VendorDataDtos.VendorDataResponse>> updateVendorData(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId,
            @PathVariable("dataId") Long dataId,
            @Valid @RequestBody VendorDataDtos.VendorDataUpdateRequest request) {
        VendorDataDtos.VendorDataResponse updated = vendorDataService.update(id, vendorId, dataId, request);
        return ResponseEntity.ok(ApiResponse.success("Vendor data updated", updated));
    }

    @DeleteMapping("/{id:\\d+}/vendors/{vendorId:\\d+}/vendor-data")
    @Operation(summary = "Delete vendor data",
            description = "Removes vendor data (URLs) for the vendor.")
    public ResponseEntity<ApiResponse<Void>> deleteVendorData(
            @PathVariable("id") Long id,
            @PathVariable("vendorId") Long vendorId) {
        vendorDataService.delete(id, vendorId);
        return ResponseEntity.ok(ApiResponse.success("Vendor data deleted"));
    }

    @GetMapping("/{id:\\d+}/client-data")
    @Operation(summary = "Get client data (PDF URLs)",
            description = "Returns quote format and client contract format PDF URLs for the organization. Returns null if none exists.")
    public ResponseEntity<ApiResponse<ClientDataDtos.ClientDataResponse>> getClientData(@PathVariable("id") Long id) {
        return ResponseEntity.ok(ApiResponse.success("Client data fetched", clientDataService.getByOrganization(id)));
    }

    @PostMapping(value = "/{id:\\d+}/client-data/quote-format/upload", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @Operation(summary = "Upload quote format PDF",
            description = "Uploads a PDF to Azure Blob Storage and saves the public URL. Replaces existing if any. Max 10MB.")
    public ResponseEntity<ApiResponse<ClientDataDtos.ClientDataResponse>> uploadQuoteFormat(
            @PathVariable("id") Long id,
            @RequestParam("file") MultipartFile file) {
        try {
            ClientDataDtos.ClientDataResponse result = clientDataService.uploadQuoteFormat(id, file);
            return ResponseEntity.status(201).body(ApiResponse.success("Quote format PDF uploaded", result));
        } catch (IllegalStateException e) {
            return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE)
                    .body(ApiResponse.error("Azure Blob Storage is not configured. Set AZURE_STORAGE_BLOB_CONNECTION_STRING."));
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(ApiResponse.error(e.getMessage()));
        }
    }

    @PostMapping(value = "/{id:\\d+}/client-data/client-contract-format/upload", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @Operation(summary = "Upload client contract format PDF",
            description = "Uploads a PDF to Azure Blob Storage and saves the public URL. Replaces existing if any. Max 10MB.")
    public ResponseEntity<ApiResponse<ClientDataDtos.ClientDataResponse>> uploadClientContractFormat(
            @PathVariable("id") Long id,
            @RequestParam("file") MultipartFile file) {
        try {
            ClientDataDtos.ClientDataResponse result = clientDataService.uploadClientContractFormat(id, file);
            return ResponseEntity.status(201).body(ApiResponse.success("Client contract format PDF uploaded", result));
        } catch (IllegalStateException e) {
            return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE)
                    .body(ApiResponse.error("Azure Blob Storage is not configured. Set AZURE_STORAGE_BLOB_CONNECTION_STRING."));
        } catch (Exception e) {
            return ResponseEntity.badRequest().body(ApiResponse.error(e.getMessage()));
        }
    }

    @DeleteMapping("/{id:\\d+}/client-data/quote-format")
    @Operation(summary = "Remove quote format PDF",
            description = "Removes the quote format PDF URL and deletes the file from Azure Blob Storage.")
    public ResponseEntity<ApiResponse<ClientDataDtos.ClientDataResponse>> removeQuoteFormat(@PathVariable("id") Long id) {
        return ResponseEntity.ok(ApiResponse.success("Quote format removed", clientDataService.removeQuoteFormat(id)));
    }

    @DeleteMapping("/{id:\\d+}/client-data/client-contract-format")
    @Operation(summary = "Remove client contract format PDF",
            description = "Removes the client contract format PDF URL and deletes the file from Azure Blob Storage.")
    public ResponseEntity<ApiResponse<ClientDataDtos.ClientDataResponse>> removeClientContractFormat(@PathVariable("id") Long id) {
        return ResponseEntity.ok(ApiResponse.success("Client contract format removed", clientDataService.removeClientContractFormat(id)));
    }

    @DeleteMapping("/{id:\\d+}/client-data")
    @Operation(summary = "Delete client data",
            description = "Removes all client data (both PDFs) for the organization.")
    public ResponseEntity<ApiResponse<Void>> deleteClientData(@PathVariable("id") Long id) {
        clientDataService.delete(id);
        return ResponseEntity.ok(ApiResponse.success("Client data deleted"));
    }

    @PutMapping("/{id}")
    @Operation(summary = "Update organization")
    public ResponseEntity<ApiResponse<OrganizationDtos.OrganizationResponse>> update(
            @PathVariable("id") Long id,
            @Valid @RequestBody OrganizationDtos.OrganizationRequest request) {
        return ResponseEntity.ok(ApiResponse.success("Organization updated", organizationService.update(id, request)));
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "Delete organization")
    public ResponseEntity<ApiResponse<Void>> delete(@PathVariable("id") Long id) {
        organizationService.delete(id);
        return ResponseEntity.ok(ApiResponse.success("Organization deleted"));
    }

    private String getCurrentUserEmail() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.getPrincipal() instanceof org.springframework.security.core.userdetails.UserDetails) {
            return ((org.springframework.security.core.userdetails.UserDetails) authentication.getPrincipal()).getUsername();
        }
        throw new com.brideside.crm.exception.UnauthorizedException("User not authenticated");
    }
}


