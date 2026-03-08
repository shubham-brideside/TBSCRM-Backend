package com.brideside.crm.service;

import com.brideside.crm.dto.EventPricingDtos;

import java.util.List;

public interface EventPricingService {

    /**
     * Get event pricing for a vendor, grouped by session.
     */
    void populateEventPricingForVendorResponse(Long vendorId, com.brideside.crm.dto.BridesideVendorDtos.VendorResponse response);

    /**
     * Save event pricing for a vendor. Replaces all existing pricing for the vendor.
     */
    void saveEventPricing(Long organizationId, Long vendorId, EventPricingDtos.EventPricingUpdateRequest request);
}
