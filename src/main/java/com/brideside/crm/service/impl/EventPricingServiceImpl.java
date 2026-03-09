package com.brideside.crm.service.impl;

import com.brideside.crm.dto.BridesideVendorDtos;
import com.brideside.crm.dto.EventPricingDtos;
import com.brideside.crm.entity.BridesideVendor;
import com.brideside.crm.entity.EventPricing;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.BridesideVendorRepository;
import com.brideside.crm.repository.EventPricingRepository;
import com.brideside.crm.service.EventPricingService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@Service
@Transactional
public class EventPricingServiceImpl implements EventPricingService {

    public static final String SESSION_DEFAULT = "DEFAULT";
    public static final String SESSION_CURRENT = "CURRENT";
    public static final String SESSION_UPCOMING = "UPCOMING";
    public static final String LEVEL_PRIMARY = "PRIMARY";
    public static final String LEVEL_SENIOR = "SENIOR";
    public static final String LEVEL_JUNIOR = "JUNIOR";

    private final EventPricingRepository eventPricingRepository;
    private final BridesideVendorRepository bridesideVendorRepository;

    public EventPricingServiceImpl(EventPricingRepository eventPricingRepository,
                                   BridesideVendorRepository bridesideVendorRepository) {
        this.eventPricingRepository = eventPricingRepository;
        this.bridesideVendorRepository = bridesideVendorRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public void populateEventPricingForVendorResponse(Long vendorId, BridesideVendorDtos.VendorResponse response) {
        List<EventPricing> all = eventPricingRepository.findByVendor_IdOrderBySessionAscArtistLevelAscDisplayOrderAscEventCodeAsc(vendorId);
        response.setEventPricing(buildRowsForSession(all, SESSION_DEFAULT));
        response.setEventPricingCurrent(buildRowsForSession(all, SESSION_CURRENT));
        response.setEventPricingUpcoming(buildRowsForSession(all, SESSION_UPCOMING));
    }

    @Override
    public void saveEventPricing(Long organizationId, Long vendorId, EventPricingDtos.EventPricingUpdateRequest request) {
        if (organizationId == null || vendorId == null) {
            throw new BadRequestException("Organization id and vendor id are required");
        }
        BridesideVendor vendor = bridesideVendorRepository.findByIdAndOrganization_Id(vendorId, organizationId)
                .orElseThrow(() -> new ResourceNotFoundException("Vendor not found with id " + vendorId + " for organization " + organizationId));

        eventPricingRepository.deleteByVendor_Id(vendorId);

        if (request != null) {
            saveRows(vendor, SESSION_DEFAULT, request.getEventPricing());
            saveRows(vendor, SESSION_CURRENT, request.getEventPricingCurrent());
            saveRows(vendor, SESSION_UPCOMING, request.getEventPricingUpcoming());
        }
    }

    private List<EventPricingDtos.EventPricingRow> buildRowsForSession(List<EventPricing> all, String session) {
        Map<String, EventPricingDtos.EventPricingRow> byCode = new LinkedHashMap<>();
        for (EventPricing ep : all) {
            if (!session.equals(ep.getSession())) continue;
            String code = ep.getEventCode();
            EventPricingDtos.EventPricingRow row = byCode.computeIfAbsent(code, k -> {
                EventPricingDtos.EventPricingRow r = new EventPricingDtos.EventPricingRow();
                r.setCode(code);
                r.setLabel(ep.getEventLabel() != null ? ep.getEventLabel() : code);
                return r;
            });
            EventPricingDtos.ArtistPricing ap = toArtistPricing(ep);
            EventPricingDtos.ArtistPricing value = isEmptyPricing(ap) ? null : ap;
            switch (ep.getArtistLevel()) {
                case LEVEL_PRIMARY -> row.setPrimary(value);
                case LEVEL_SENIOR -> row.setSenior(value);
                case LEVEL_JUNIOR -> row.setJunior(value);
                default -> { /* ignore unknown level */ }
            }
        }
        return new ArrayList<>(byCode.values());
    }

    private EventPricingDtos.ArtistPricing toArtistPricing(EventPricing ep) {
        EventPricingDtos.ArtistPricing ap = new EventPricingDtos.ArtistPricing();
        ap.setBasePrice(ep.getBasePrice());
        ap.setDestinationPrice(ep.getDestinationPrice());
        ap.setAdditionalMakeupPrice(ep.getAdditionalMakeupPrice());
        ap.setAvailabilityAtStudio(ep.getAvailabilityAtStudio());
        ap.setPolicyNotes(ep.getPolicyNotes());
        ap.setCurrency(ep.getCurrency());
        return ap;
    }

    private boolean isEmptyPricing(EventPricingDtos.ArtistPricing ap) {
        if (ap == null) return true;
        return ap.getBasePrice() == null && ap.getDestinationPrice() == null
                && ap.getAdditionalMakeupPrice() == null
                && (ap.getAvailabilityAtStudio() == null || ap.getAvailabilityAtStudio().isBlank())
                && (ap.getPolicyNotes() == null || ap.getPolicyNotes().isBlank())
                && (ap.getCurrency() == null || ap.getCurrency().isBlank());
    }

    private void saveRows(BridesideVendor vendor, String session, List<EventPricingDtos.EventPricingRow> rows) {
        if (rows == null || rows.isEmpty()) return;
        int order = 0;
        for (EventPricingDtos.EventPricingRow row : rows) {
            String code = trimmed(row.getCode());
            if (code == null || code.isBlank()) continue;
            String label = trimmed(row.getLabel());
            if (label == null) label = code;

            boolean hasAnyPricing = row.getPrimary() != null || row.getSenior() != null || row.getJunior() != null;
            if (hasAnyPricing) {
                if (row.getPrimary() != null) {
                    eventPricingRepository.save(toEntity(vendor, session, LEVEL_PRIMARY, code, label, order, row.getPrimary()));
                }
                if (row.getSenior() != null) {
                    eventPricingRepository.save(toEntity(vendor, session, LEVEL_SENIOR, code, label, order, row.getSenior()));
                }
                if (row.getJunior() != null) {
                    eventPricingRepository.save(toEntity(vendor, session, LEVEL_JUNIOR, code, label, order, row.getJunior()));
                }
            } else {
                // Event without pricing details: save as PRIMARY placeholder with null pricing so it persists and is editable
                eventPricingRepository.save(toEntity(vendor, session, LEVEL_PRIMARY, code, label, order, null));
            }
            order++;
        }
    }

    private EventPricing toEntity(BridesideVendor vendor, String session, String artistLevel,
                                 String code, String label, int displayOrder, EventPricingDtos.ArtistPricing ap) {
        EventPricing ep = new EventPricing();
        ep.setVendor(vendor);
        ep.setSession(session);
        ep.setArtistLevel(artistLevel);
        ep.setEventCode(code);
        ep.setEventLabel(label);
        ep.setDisplayOrder(displayOrder);
        if (ap != null) {
            ep.setBasePrice(ap.getBasePrice());
            ep.setDestinationPrice(ap.getDestinationPrice());
            ep.setAdditionalMakeupPrice(ap.getAdditionalMakeupPrice());
            ep.setAvailabilityAtStudio(trimmed(ap.getAvailabilityAtStudio()));
            ep.setPolicyNotes(trimmed(ap.getPolicyNotes()));
            ep.setCurrency(trimmed(ap.getCurrency()));
        } else {
            ep.setBasePrice(null);
            ep.setDestinationPrice(null);
            ep.setAdditionalMakeupPrice(null);
            ep.setAvailabilityAtStudio(null);
            ep.setPolicyNotes(null);
            ep.setCurrency(null);
        }
        return ep;
    }

    private String trimmed(String value) {
        return value == null ? null : value.trim();
    }
}
