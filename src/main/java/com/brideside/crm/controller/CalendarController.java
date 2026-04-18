package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.CalendarDtos;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.DealCalendarEventType;
import com.brideside.crm.entity.DealStatus;
import com.brideside.crm.entity.VendorCalendarEvent;
import com.brideside.crm.integration.calendar.GoogleCalendarService;
import com.brideside.crm.repository.DealCalendarEventTypeRepository;
import com.brideside.crm.repository.DealRepository;
import com.brideside.crm.repository.VendorCalendarEventRepository;
import com.brideside.crm.service.VendorCalendarSyncService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.DeleteMapping;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/calendar")
@Tag(name = "Calendar", description = "Calendar aggregation APIs")
public class CalendarController {

    private final VendorCalendarEventRepository vendorCalendarEventRepository;
    private final DealRepository dealRepository;
    private final DealCalendarEventTypeRepository dealCalendarEventTypeRepository;
    private final ObjectMapper objectMapper;
    private final Optional<GoogleCalendarService> googleCalendarService;
    private final Optional<VendorCalendarSyncService> vendorCalendarSyncService;

    public CalendarController(VendorCalendarEventRepository vendorCalendarEventRepository,
                              DealRepository dealRepository,
                              DealCalendarEventTypeRepository dealCalendarEventTypeRepository,
                              ObjectMapper objectMapper,
                              Optional<GoogleCalendarService> googleCalendarService,
                              Optional<VendorCalendarSyncService> vendorCalendarSyncService) {
        this.vendorCalendarEventRepository = vendorCalendarEventRepository;
        this.dealRepository = dealRepository;
        this.dealCalendarEventTypeRepository = dealCalendarEventTypeRepository;
        this.objectMapper = objectMapper;
        this.googleCalendarService = googleCalendarService;
        this.vendorCalendarSyncService = vendorCalendarSyncService;
    }

    @GetMapping("/vendor-events")
    @Operation(summary = "List vendor Google Calendar events mirrored in CRM")
    public ResponseEntity<ApiResponse<List<CalendarDtos.VendorEventResponse>>> listVendorEvents(
            @RequestParam(value = "organizationId", required = false) Long organizationId,
            @RequestParam(value = "from", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) Instant from,
            @RequestParam(value = "to", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) Instant to) {

        Instant fallbackFrom = from != null ? from : Instant.now().minus(30, ChronoUnit.DAYS);
        Instant fallbackTo = to != null ? to : Instant.now().plus(120, ChronoUnit.DAYS);

        List<VendorCalendarEvent> events;
        events = organizationId != null
                ? vendorCalendarEventRepository.findByOrganization_IdAndStartAtBetween(organizationId, fallbackFrom, fallbackTo)
                : vendorCalendarEventRepository.findByStartAtBetween(fallbackFrom, fallbackTo);

        List<CalendarDtos.VendorEventResponse> response = events.stream()
                .map(this::toDto)
                .collect(Collectors.toList());
        return ResponseEntity.ok(ApiResponse.success("Vendor calendar events fetched", response));
    }

    @GetMapping("/deals/{dealId}/event-types-by-date")
    @Operation(summary = "Get calendar event types by date for a deal")
    public ResponseEntity<ApiResponse<List<CalendarDtos.DealEventTypeByDateItem>>> getDealEventTypesByDate(
            @PathVariable("dealId") Long dealId) {
        if (dealId == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("dealId is required"));
        }
        List<CalendarDtos.DealEventTypeByDateItem> items = dealCalendarEventTypeRepository
                .findByDeal_IdOrderByEventDateAsc(dealId)
                .stream()
                .map(this::toDealEventTypeItem)
                .collect(Collectors.toList());
        return ResponseEntity.ok(ApiResponse.success("Deal event type mapping fetched", items));
    }

    @PostMapping("/deals/{dealId}/event-types-by-date")
    @Transactional
    @Operation(summary = "Upsert calendar event types by date for a deal")
    public ResponseEntity<ApiResponse<List<CalendarDtos.DealEventTypeByDateItem>>> upsertDealEventTypesByDate(
            @PathVariable("dealId") Long dealId,
            @RequestBody CalendarDtos.UpsertDealEventTypesByDateRequest request) {
        if (dealId == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("dealId is required"));
        }
        if (request == null || request.items == null || request.items.isEmpty()) {
            return ResponseEntity.badRequest().body(ApiResponse.error("items are required"));
        }

        Deal deal = dealRepository.findById(dealId)
                .orElseThrow(() -> new RuntimeException("Deal not found"));

        dealCalendarEventTypeRepository.deleteAllByDealId(dealId);
        dealCalendarEventTypeRepository.flush();
        List<DealCalendarEventType> entitiesToSave = new ArrayList<>();
        Set<LocalDate> seenDates = new HashSet<>();

        for (CalendarDtos.DealEventTypeByDateItem item : request.items) {
            if (item == null || !StringUtils.hasText(item.eventDate) || !StringUtils.hasText(item.eventType)) {
                return ResponseEntity.badRequest().body(ApiResponse.error("Each item must include eventDate and eventType"));
            }
            LocalDate parsedDate;
            try {
                parsedDate = LocalDate.parse(item.eventDate.trim());
            } catch (Exception ex) {
                return ResponseEntity.badRequest().body(ApiResponse.error("Invalid eventDate format. Use yyyy-MM-dd"));
            }
            if (!seenDates.add(parsedDate)) {
                return ResponseEntity.badRequest().body(ApiResponse.error("Duplicate eventDate in request: " + parsedDate));
            }

            DealCalendarEventType entity = new DealCalendarEventType();
            entity.setDeal(deal);
            entity.setEventDate(parsedDate);
            entity.setEventType(item.eventType.trim());
            entity.setTeam(normalizeTeam(item.team));
            entitiesToSave.add(entity);
        }

        List<DealCalendarEventType> savedMappings = dealCalendarEventTypeRepository.saveAll(entitiesToSave);
        syncDealEventsIfEnabled(deal);
        mirrorVendorEventsIfEnabled();
        applyTeamsToVendorEvents(dealId, savedMappings);

        List<CalendarDtos.DealEventTypeByDateItem> response = savedMappings.stream()
                .map(this::toDealEventTypeItem)
                .collect(Collectors.toList());
        return ResponseEntity.ok(ApiResponse.success("Deal event type mapping saved", response));
    }

    @DeleteMapping("/deals/{dealId}/event-types-by-date")
    @Transactional
    @Operation(summary = "Delete calendar event type mapping for a deal")
    public ResponseEntity<ApiResponse<Void>> deleteDealEventTypesByDate(
            @PathVariable("dealId") Long dealId) {
        if (dealId == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("dealId is required"));
        }

        Deal deal = dealRepository.findById(dealId)
                .orElseThrow(() -> new RuntimeException("Deal not found"));

        dealCalendarEventTypeRepository.deleteAllByDealId(dealId);
        dealCalendarEventTypeRepository.flush();

        // Re-sync so Google Calendar reflects fallback behavior after mapping removal.
        syncDealEventsIfEnabled(deal);

        return ResponseEntity.ok(ApiResponse.success("Deal event type mapping deleted"));
    }

    @DeleteMapping("/vendor-events/deals/{dealId}")
    @Transactional
    @Operation(summary = "Delete mirrored vendor calendar events by deal id (and from Google when enabled)")
    public ResponseEntity<ApiResponse<Map<String, Object>>> deleteVendorEventsByDealId(
            @PathVariable("dealId") Long dealId) {
        if (dealId == null) {
            return ResponseEntity.badRequest().body(ApiResponse.error("dealId is required"));
        }
        List<VendorCalendarEvent> existing = vendorCalendarEventRepository.findByDealId(dealId);
        int googleDeleteAttempts = 0;
        if (googleCalendarService.isPresent()) {
            for (VendorCalendarEvent event : existing) {
                if (StringUtils.hasText(event.getGoogleCalendarId()) && StringUtils.hasText(event.getGoogleEventId())) {
                    googleCalendarService.get().deleteEventById(event.getGoogleCalendarId(), event.getGoogleEventId());
                    googleDeleteAttempts++;
                }
            }
        }
        int deleted = vendorCalendarEventRepository.deleteByDealId(dealId);
        return ResponseEntity.ok(ApiResponse.success(
                "Vendor calendar events deleted by deal id",
                Map.of(
                        "dealId", dealId,
                        "googleDeleteAttempts", googleDeleteAttempts,
                        "googleDeleteEnabled", googleCalendarService.isPresent(),
                        "deletedCount", deleted
                )
        ));
    }

    private CalendarDtos.VendorEventResponse toDto(VendorCalendarEvent event) {
        CalendarDtos.VendorEventResponse dto = new CalendarDtos.VendorEventResponse();
        dto.id = event.getId();
        dto.dealId = event.getDealId();
        dto.team = event.getTeam();
        dto.organizationId = event.getOrganization() != null ? event.getOrganization().getId() : null;
        dto.organizationName = event.getOrganization() != null ? event.getOrganization().getName() : null;
        dto.googleEventId = event.getGoogleEventId();
        dto.summary = event.getSummary();
        dto.description = event.getDescription();
        dto.startAt = event.getStartAt();
        dto.endAt = event.getEndAt();
        dto.allDay = event.isAllDay();
        dto.status = event.getStatus();
        dto.lastSyncedAt = event.getLastSyncedAt();
        return dto;
    }

    private CalendarDtos.DealEventTypeByDateItem toDealEventTypeItem(DealCalendarEventType entity) {
        CalendarDtos.DealEventTypeByDateItem item = new CalendarDtos.DealEventTypeByDateItem();
        item.eventDate = entity.getEventDate() != null ? entity.getEventDate().toString() : null;
        item.eventType = entity.getEventType();
        item.team = entity.getTeam();
        return item;
    }

    private void syncDealEventsIfEnabled(Deal deal) {
        if (deal == null || googleCalendarService.isEmpty()) {
            return;
        }
        if (deal.getStatus() != DealStatus.WON) {
            googleCalendarService.get().deleteDealEvents(deal);
            deal.setGoogleCalendarEventIds(null);
            deal.setGoogleCalendarEventId(null);
            dealRepository.save(deal);
            return;
        }
        googleCalendarService.get().upsertDealEvents(deal).ifPresent(map -> persistGoogleEventIds(deal, map));
    }

    private void persistGoogleEventIds(Deal deal, Map<String, String> map) {
        try {
            String eventIdsJson = objectMapper.writeValueAsString(map);
            deal.setGoogleCalendarEventIds(eventIdsJson);
            if (!map.isEmpty()) {
                deal.setGoogleCalendarEventId(map.values().iterator().next());
            } else {
                deal.setGoogleCalendarEventId(null);
            }
            dealRepository.save(deal);
        } catch (JsonProcessingException ignored) {
            // Keep API successful even if legacy event-id persistence fails.
        }
    }

    private void mirrorVendorEventsIfEnabled() {
        if (vendorCalendarSyncService.isPresent()) {
            vendorCalendarSyncService.get().syncAllVendorCalendars();
        }
    }

    private void applyTeamsToVendorEvents(Long dealId, List<DealCalendarEventType> mappings) {
        if (dealId == null || mappings == null || mappings.isEmpty()) {
            return;
        }
        Map<LocalDate, String> teamByDate = new HashMap<>();
        for (DealCalendarEventType mapping : mappings) {
            if (mapping.getEventDate() != null) {
                teamByDate.put(mapping.getEventDate(), normalizeTeam(mapping.getTeam()));
            }
        }
        if (teamByDate.isEmpty()) {
            return;
        }
        List<VendorCalendarEvent> vendorEvents = vendorCalendarEventRepository.findByDealId(dealId);
        if (vendorEvents.isEmpty()) {
            return;
        }
        for (VendorCalendarEvent event : vendorEvents) {
            if (event.getStartAt() == null) {
                continue;
            }
            LocalDate eventDate = event.getStartAt().atZone(ZoneOffset.UTC).toLocalDate();
            if (teamByDate.containsKey(eventDate)) {
                event.setTeam(teamByDate.get(eventDate));
            }
        }
        vendorCalendarEventRepository.saveAll(vendorEvents);
    }

    private String normalizeTeam(String team) {
        if (!StringUtils.hasText(team)) {
            return null;
        }
        return team.trim();
    }
}

