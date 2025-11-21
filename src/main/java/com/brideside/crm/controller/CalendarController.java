package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.CalendarDtos;
import com.brideside.crm.entity.VendorCalendarEvent;
import com.brideside.crm.repository.VendorCalendarEventRepository;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/calendar")
@Tag(name = "Calendar", description = "Calendar aggregation APIs")
public class CalendarController {

    private final VendorCalendarEventRepository vendorCalendarEventRepository;

    public CalendarController(VendorCalendarEventRepository vendorCalendarEventRepository) {
        this.vendorCalendarEventRepository = vendorCalendarEventRepository;
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

    private CalendarDtos.VendorEventResponse toDto(VendorCalendarEvent event) {
        CalendarDtos.VendorEventResponse dto = new CalendarDtos.VendorEventResponse();
        dto.id = event.getId();
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
}

