package com.brideside.crm.service.impl;

import com.brideside.crm.config.GoogleCalendarProperties;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.VendorCalendarEvent;
import com.brideside.crm.integration.calendar.GoogleCalendarService;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.repository.VendorCalendarEventRepository;
import com.brideside.crm.service.VendorCalendarSyncService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

@Service
@ConditionalOnProperty(prefix = "google.calendar", name = "enabled", havingValue = "true")
public class VendorCalendarSyncServiceImpl implements VendorCalendarSyncService {

    private static final Logger log = LoggerFactory.getLogger(VendorCalendarSyncServiceImpl.class);

    private final GoogleCalendarService googleCalendarService;
    private final OrganizationRepository organizationRepository;
    private final VendorCalendarEventRepository eventRepository;
    private final GoogleCalendarProperties properties;

    public VendorCalendarSyncServiceImpl(GoogleCalendarService googleCalendarService,
                                         OrganizationRepository organizationRepository,
                                         VendorCalendarEventRepository eventRepository,
                                         GoogleCalendarProperties properties) {
        this.googleCalendarService = googleCalendarService;
        this.organizationRepository = organizationRepository;
        this.eventRepository = eventRepository;
        this.properties = properties;
    }

    @Override
    @Transactional
    public void syncAllVendorCalendars() {
        Instant now = Instant.now();
        Instant timeMin = now.minus(properties.getSyncWindowPastDays(), ChronoUnit.DAYS);
        Instant timeMax = now.plus(properties.getSyncWindowFutureDays(), ChronoUnit.DAYS);
        List<Organization> organizations = organizationRepository.findByGoogleCalendarIdIsNotNull();
        if (CollectionUtils.isEmpty(organizations)) {
            return;
        }
        organizations.forEach(organization -> syncOrganizationCalendar(organization, timeMin, timeMax, now));
    }

    @Scheduled(fixedDelayString = "${google.calendar.poll-interval-minutes:10}",
            initialDelayString = "${google.calendar.poll-interval-minutes:10}",
            timeUnit = TimeUnit.MINUTES)
    public void scheduledSync() {
        try {
            syncAllVendorCalendars();
        } catch (Exception ex) {
            log.warn("Vendor calendar sync failed: {}", ex.getMessage(), ex);
        }
    }

    private void syncOrganizationCalendar(Organization organization, Instant timeMin, Instant timeMax, Instant syncedAt) {
        String calendarId = organization.getGoogleCalendarId();
        if (calendarId == null || calendarId.isBlank()) {
            return;
        }
        List<GoogleCalendarService.GoogleCalendarEventPayload> googleEvents =
                googleCalendarService.fetchEvents(calendarId, timeMin, timeMax);
        Set<String> seen = new HashSet<>();
        for (GoogleCalendarService.GoogleCalendarEventPayload payload : googleEvents) {
            seen.add(payload.eventId());
            if ("cancelled".equalsIgnoreCase(payload.status())) {
                eventRepository.findByGoogleEventId(payload.eventId())
                        .ifPresent(eventRepository::delete);
                continue;
            }
            VendorCalendarEvent event = eventRepository.findByGoogleEventId(payload.eventId())
                    .orElseGet(VendorCalendarEvent::new);
            event.setOrganization(organization);
            event.setGoogleCalendarId(calendarId);
            event.setGoogleEventId(payload.eventId());
            event.setSummary(payload.summary());
            event.setDescription(payload.description());
            event.setDealId(extractDealId(payload.description()));
            event.setStartAt(payload.startAt());
            event.setEndAt(payload.endAt());
            event.setAllDay(payload.allDay());
            event.setStatus(payload.status());
            event.setLastSyncedAt(syncedAt);
            eventRepository.save(event);
        }
        List<VendorCalendarEvent> existingInWindow =
                eventRepository.findByOrganization_IdAndStartAtBetween(organization.getId(), timeMin, timeMax);
        existingInWindow.stream()
                .filter(event -> !seen.contains(event.getGoogleEventId()))
                .forEach(eventRepository::delete);
    }

    private Long extractDealId(String description) {
        if (description == null || description.isBlank()) {
            return null;
        }
        String marker = "Deal ID:";
        int markerIndex = description.lastIndexOf(marker);
        if (markerIndex < 0) {
            return null;
        }
        String trailing = description.substring(markerIndex + marker.length()).trim();
        if (trailing.isEmpty()) {
            return null;
        }
        StringBuilder digits = new StringBuilder();
        for (int i = 0; i < trailing.length(); i++) {
            char ch = trailing.charAt(i);
            if (Character.isDigit(ch)) {
                digits.append(ch);
            } else if (digits.length() > 0) {
                break;
            }
        }
        if (digits.length() == 0) {
            return null;
        }
        try {
            return Long.parseLong(digits.toString());
        } catch (NumberFormatException ex) {
            return null;
        }
    }
}

