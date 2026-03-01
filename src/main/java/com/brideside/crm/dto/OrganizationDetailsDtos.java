package com.brideside.crm.dto;

import java.util.List;

public final class OrganizationDetailsDtos {
    private OrganizationDetailsDtos() {
    }

    public static class OrganizationWithDetailsResponse {
        private OrganizationDtos.OrganizationResponse organization;
        private List<BridesideVendorDtos.VendorResponse> vendors;

        public OrganizationDtos.OrganizationResponse getOrganization() { return organization; }
        public void setOrganization(OrganizationDtos.OrganizationResponse organization) { this.organization = organization; }
        public List<BridesideVendorDtos.VendorResponse> getVendors() { return vendors; }
        public void setVendors(List<BridesideVendorDtos.VendorResponse> vendors) { this.vendors = vendors; }
    }
}

