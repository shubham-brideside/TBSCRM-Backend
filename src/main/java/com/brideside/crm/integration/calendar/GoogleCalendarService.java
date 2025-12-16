package com.brideside.crm.integration.calendar;

import com.brideside.crm.config.GoogleCalendarProperties;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.Organization;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.auth.oauth2.GoogleCredentials;
import com.google.auth.oauth2.ServiceAccountCredentials;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import com.fasterxml.jackson.core.type.TypeReference;

@Service
@ConditionalOnProperty(prefix = "google.calendar", name = "enabled", havingValue = "true")
public class GoogleCalendarService {

    private static final Logger log = LoggerFactory.getLogger(GoogleCalendarService.class);
    private static final String EVENTS_URL = "https://www.googleapis.com/calendar/v3/calendars/%s/events";
    private static final String EVENT_URL = EVENTS_URL + "/%s";

    private final GoogleCalendarProperties properties;
    private final ObjectMapper objectMapper;
    private final HttpClient httpClient;
    private final GoogleCredentials credentials;

    public GoogleCalendarService(GoogleCalendarProperties properties, ObjectMapper objectMapper) throws IOException {
        this.properties = properties;
        this.objectMapper = objectMapper;
        
        // Debug logging to help troubleshoot credential loading
        log.info("Google Calendar Service initializing...");
        log.info("Google Calendar enabled: {}", properties.isEnabled());
        log.info("Credentials file configured: {}", StringUtils.hasText(properties.getCredentialsFile()));
        log.info("Credentials JSON configured: {}", StringUtils.hasText(properties.getCredentialsJson()));
        if (StringUtils.hasText(properties.getCredentialsJson())) {
            log.info("Credentials JSON length: {} characters", properties.getCredentialsJson().length());
            String jsonPreview = properties.getCredentialsJson().substring(0, Math.min(100, properties.getCredentialsJson().length()));
            log.info("Credentials JSON preview: {}...", jsonPreview);
        }
        
        if (!properties.hasCredentialSource()) {
            log.error("Google Calendar credentials are not configured. Set GOOGLE_CALENDAR_CREDENTIALS_JSON or GOOGLE_CALENDAR_CREDENTIALS_FILE.");
            throw new IllegalStateException("Google Calendar credentials are not configured. Please set GOOGLE_CALENDAR_CREDENTIALS_JSON environment variable or GOOGLE_CALENDAR_CREDENTIALS_FILE property.");
        }
        
        try {
            this.credentials = buildGoogleCredentials();
            log.info("Google Calendar credentials loaded successfully");
        } catch (Exception e) {
            log.error("Failed to load Google Calendar credentials: {}", e.getMessage(), e);
            throw new IOException("Failed to load Google Calendar credentials: " + e.getMessage(), e);
        }
        
        this.httpClient = HttpClient.newHttpClient();
    }

    /**
     * Upserts multiple calendar events for a deal (one per event date).
     * Returns a map of date strings to event IDs.
     */
    public Optional<Map<String, String>> upsertDealEvents(Deal deal) {
        if (!shouldSync(deal)) {
            return Optional.empty();
        }
        
        List<LocalDate> eventDates = getAllEventDates(deal);
        if (eventDates == null || eventDates.isEmpty()) {
            return Optional.empty();
        }
        
        String calendarId = deal.getOrganization().getGoogleCalendarId().trim();
        Map<String, String> existingEventIds = parseEventIds(deal);
        Map<String, String> updatedEventIds = new HashMap<>();
        
        for (LocalDate eventDate : eventDates) {
            String dateStr = eventDate.toString();
            String existingEventId = existingEventIds != null ? existingEventIds.get(dateStr) : null;
            
            Optional<String> eventId = upsertSingleEvent(deal, calendarId, eventDate, existingEventId);
            if (eventId.isPresent()) {
                updatedEventIds.put(dateStr, eventId.get());
            }
        }
        
        // Delete events for dates that are no longer in the list
        if (existingEventIds != null) {
            for (Map.Entry<String, String> entry : existingEventIds.entrySet()) {
                if (!updatedEventIds.containsKey(entry.getKey())) {
                    deleteSingleEvent(calendarId, entry.getValue());
                }
            }
        }
        
        return updatedEventIds.isEmpty() ? Optional.empty() : Optional.of(updatedEventIds);
    }
    
    /**
     * Upserts a single calendar event for a specific date.
     */
    private Optional<String> upsertSingleEvent(Deal deal, String calendarId, LocalDate eventDate, String existingEventId) {
        ObjectNode payload = buildEventPayload(deal, eventDate);
        String body;
        try {
            body = objectMapper.writeValueAsString(payload);
        } catch (IOException e) {
            log.warn("Failed to serialize calendar payload for deal {} date {}: {}", deal.getId(), eventDate, e.getMessage(), e);
            return Optional.empty();
        }

        boolean updatingExisting = StringUtils.hasText(existingEventId);
        String endpoint = updatingExisting
                ? EVENT_URL.formatted(encode(calendarId), encode(existingEventId))
                : EVENTS_URL.formatted(encode(calendarId));

        HttpRequest.Builder builder = HttpRequest.newBuilder()
                .uri(URI.create(endpoint))
                .header("Authorization", "Bearer " + fetchAccessToken())
                .header("Content-Type", "application/json");

        HttpRequest request = updatingExisting
                ? builder.PUT(HttpRequest.BodyPublishers.ofString(body)).build()
                : builder.POST(HttpRequest.BodyPublishers.ofString(body)).build();

        try {
            HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            if (response.statusCode() >= 200 && response.statusCode() < 300) {
                JsonNode node = objectMapper.readTree(response.body());
                if (!updatingExisting && node.hasNonNull("htmlLink")) {
                    System.out.println("Google Calendar event link: " + node.get("htmlLink").asText());
                }
                if (node.hasNonNull("id")) {
                    return Optional.of(node.get("id").asText());
                }
            } else {
                log.warn("Google Calendar API upsert failed for deal {} date {}: {} {}", deal.getId(), eventDate, response.statusCode(), response.body());
            }
        } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
            log.warn("Google Calendar sync interrupted for deal {} date {}: {}", deal.getId(), eventDate, ex.getMessage(), ex);
        } catch (IOException ex) {
            log.warn("Google Calendar sync failed for deal {} date {}: {}", deal.getId(), eventDate, ex.getMessage(), ex);
        }
        return Optional.empty();
    }
    
    /**
     * Legacy method for backward compatibility - uses first event date.
     * @deprecated Use upsertDealEvents instead
     */
    @Deprecated
    public Optional<String> upsertDealEvent(Deal deal) {
        if (!shouldSync(deal)) {
            return Optional.empty();
        }
        LocalDate firstDate = getFirstEventDate(deal);
        if (firstDate == null) {
            return Optional.empty();
        }
        String calendarId = deal.getOrganization().getGoogleCalendarId().trim();
        String existingEventId = deal.getGoogleCalendarEventId();
        return upsertSingleEvent(deal, calendarId, firstDate, existingEventId);
    }

    /**
     * Deletes all calendar events for a deal.
     */
    public void deleteDealEvents(Deal deal) {
        if (deal == null || deal.getOrganization() == null) {
            return;
        }
        String calendarId = deal.getOrganization().getGoogleCalendarId();
        if (!StringUtils.hasText(calendarId)) {
            return;
        }

        // Delete events from google_calendar_event_ids
        Map<String, String> eventIds = parseEventIds(deal);
        if (eventIds != null && !eventIds.isEmpty()) {
            for (String eventId : eventIds.values()) {
                if (StringUtils.hasText(eventId)) {
                    deleteSingleEvent(calendarId.trim(), eventId);
                }
            }
        }
        
        // Also delete legacy single event if it exists
        if (StringUtils.hasText(deal.getGoogleCalendarEventId())) {
            deleteSingleEvent(calendarId.trim(), deal.getGoogleCalendarEventId());
        }
    }
    
    /**
     * Deletes a single calendar event.
     */
    private void deleteSingleEvent(String calendarId, String eventId) {
        if (!StringUtils.hasText(calendarId) || !StringUtils.hasText(eventId)) {
            return;
        }

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(EVENT_URL.formatted(encode(calendarId.trim()), encode(eventId))))
                .header("Authorization", "Bearer " + fetchAccessToken())
                .DELETE()
                .build();

        try {
            HttpResponse<Void> response = httpClient.send(request, HttpResponse.BodyHandlers.discarding());
            if (response.statusCode() >= 400) {
                log.warn("Failed to delete Google Calendar event {}: status {}", eventId, response.statusCode());
            }
        } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
            log.warn("Delete Google Calendar event interrupted for event {}: {}", eventId, ex.getMessage(), ex);
        } catch (IOException ex) {
            log.warn("Unable to delete Google Calendar event {}: {}", eventId, ex.getMessage(), ex);
        }
    }
    
    /**
     * Legacy method for backward compatibility.
     * @deprecated Use deleteDealEvents instead
     */
    @Deprecated
    public void deleteDealEvent(Deal deal) {
        deleteDealEvents(deal);
    }

    public List<GoogleCalendarEventPayload> fetchEvents(String calendarId, Instant timeMin, Instant timeMax) {
        if (!StringUtils.hasText(calendarId)) {
            return List.of();
        }
        String encodedCalendar = encode(calendarId.trim());
        String timeMinIso = timeMin != null ? timeMin.toString() : null;
        String timeMaxIso = timeMax != null ? timeMax.toString() : null;
        List<GoogleCalendarEventPayload> events = new ArrayList<>();
        String pageToken = null;
        do {
            StringBuilder uriBuilder = new StringBuilder(EVENTS_URL.formatted(encodedCalendar))
                    .append("?singleEvents=true&orderBy=startTime");
            if (timeMinIso != null) {
                uriBuilder.append("&timeMin=").append(encode(timeMinIso));
            }
            if (timeMaxIso != null) {
                uriBuilder.append("&timeMax=").append(encode(timeMaxIso));
            }
            if (pageToken != null) {
                uriBuilder.append("&pageToken=").append(encode(pageToken));
            }
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(uriBuilder.toString()))
                    .header("Authorization", "Bearer " + fetchAccessToken())
                    .GET()
                    .build();
            try {
                HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
                if (response.statusCode() >= 200 && response.statusCode() < 300) {
                    JsonNode body = objectMapper.readTree(response.body());
                    JsonNode items = body.get("items");
                    if (items != null && items.isArray()) {
                        items.forEach(item -> {
                            GoogleCalendarEventPayload payload = toPayload(calendarId, item);
                            if (payload != null) {
                                events.add(payload);
                            }
                        });
                    }
                    pageToken = body.hasNonNull("nextPageToken") ? body.get("nextPageToken").asText() : null;
                } else {
                    log.warn("Google Calendar events fetch failed for calendar {}: {} {}", calendarId, response.statusCode(), response.body());
                    break;
                }
            } catch (InterruptedException ex) {
                Thread.currentThread().interrupt();
                log.warn("Google Calendar fetch interrupted for calendar {}: {}", calendarId, ex.getMessage(), ex);
                break;
            } catch (IOException ex) {
                log.warn("Google Calendar fetch failed for calendar {}: {}", calendarId, ex.getMessage(), ex);
                break;
            }
        } while (pageToken != null);
        return events;
    }

    private boolean shouldSync(Deal deal) {
        if (deal == null) {
            return false;
        }
        // Get first event date from eventDates or fallback to legacy eventDate
        LocalDate firstEventDate = getFirstEventDate(deal);
        if (firstEventDate == null) {
            return false;
        }
        Organization organization = deal.getOrganization();
        return organization != null && StringUtils.hasText(organization.getGoogleCalendarId());
    }
    
    /**
     * Gets all event dates from the eventDates JSON array, or falls back to legacy eventDate.
     */
    private List<LocalDate> getAllEventDates(Deal deal) {
        List<LocalDate> dates = new ArrayList<>();
        
        if (deal.getEventDates() != null && !deal.getEventDates().isEmpty()) {
            try {
                List<String> dateStrings = objectMapper.readValue(
                    deal.getEventDates(),
                    new TypeReference<List<String>>() {}
                );
                if (dateStrings != null) {
                    for (String dateStr : dateStrings) {
                        try {
                            dates.add(LocalDate.parse(dateStr));
                        } catch (Exception e) {
                            // Ignore parse errors for individual dates
                        }
                    }
                }
            } catch (Exception e) {
                // If parsing fails, fallback to legacy eventDate
            }
        }
        
        // Fallback to legacy eventDate if no dates found
        if (dates.isEmpty() && deal.getEventDate() != null) {
            dates.add(deal.getEventDate());
        }
        
        return dates.isEmpty() ? null : dates;
    }
    
    /**
     * Gets the first event date from the eventDates JSON array, or falls back to legacy eventDate.
     */
    private LocalDate getFirstEventDate(Deal deal) {
        List<LocalDate> dates = getAllEventDates(deal);
        return dates != null && !dates.isEmpty() ? dates.get(0) : null;
    }
    
    /**
     * Parses the google_calendar_event_ids JSON object to a map of date strings to event IDs.
     */
    private Map<String, String> parseEventIds(Deal deal) {
        if (deal.getGoogleCalendarEventIds() == null || deal.getGoogleCalendarEventIds().isEmpty()) {
            return null;
        }
        try {
            return objectMapper.readValue(
                deal.getGoogleCalendarEventIds(),
                new TypeReference<Map<String, String>>() {}
            );
        } catch (Exception e) {
            return null;
        }
    }

    private GoogleCredentials buildGoogleCredentials() throws IOException {
        try (InputStream stream = resolveCredentialStream()) {
            GoogleCredentials creds = ServiceAccountCredentials.fromStream(stream)
                    .createScoped(Collections.singleton("https://www.googleapis.com/auth/calendar"));
            if (StringUtils.hasText(properties.getImpersonatedUser()) && creds instanceof ServiceAccountCredentials sac) {
                return sac.createDelegated(properties.getImpersonatedUser());
            }
            return creds;
        }
    }

    private InputStream resolveCredentialStream() throws IOException {
        // Priority 1: Use credentialsJson from environment variable
        if (StringUtils.hasText(properties.getCredentialsJson())) {
            String jsonContent = properties.getCredentialsJson().trim();
            log.info("Attempting to load credentials from GOOGLE_CALENDAR_CREDENTIALS_JSON environment variable");
            
            // Try to decode if it looks like base64 (optional - handles both raw JSON and base64)
            try {
                // Check if it's valid JSON by trying to parse it
                if (jsonContent.startsWith("{") && jsonContent.endsWith("}")) {
                    // It's already raw JSON
                    log.info("Using raw JSON credentials from environment variable");
                    // Validate it's valid JSON by trying to parse it
                    try {
                        objectMapper.readTree(jsonContent);
                        log.info("Credentials JSON is valid");
                    } catch (Exception e) {
                        log.error("Credentials JSON is not valid JSON: {}", e.getMessage());
                        throw new IOException("Invalid JSON in GOOGLE_CALENDAR_CREDENTIALS_JSON: " + e.getMessage(), e);
                    }
                    return new ByteArrayInputStream(jsonContent.getBytes(StandardCharsets.UTF_8));
                } else {
                    // Might be base64 encoded, try to decode
                    log.info("Attempting to decode base64 credentials");
                    byte[] decoded = java.util.Base64.getDecoder().decode(jsonContent);
                    String decodedJson = new String(decoded, StandardCharsets.UTF_8);
                    log.info("Successfully decoded base64 credentials from environment variable");
                    // Validate decoded JSON
                    try {
                        objectMapper.readTree(decodedJson);
                        log.info("Decoded credentials JSON is valid");
                    } catch (Exception e) {
                        log.error("Decoded credentials JSON is not valid: {}", e.getMessage());
                        throw new IOException("Invalid JSON after base64 decode: " + e.getMessage(), e);
                    }
                    return new ByteArrayInputStream(decodedJson.getBytes(StandardCharsets.UTF_8));
                }
            } catch (IllegalArgumentException e) {
                // Base64 decode failed, try using as raw JSON
                log.warn("Failed to decode as base64 (not base64), trying as raw JSON: {}", e.getMessage());
                if (!jsonContent.startsWith("{")) {
                    log.error("Credentials JSON doesn't start with '{' and is not valid base64. Please check your GOOGLE_CALENDAR_CREDENTIALS_JSON environment variable.");
                    throw new IOException("Invalid credentials format. Expected JSON starting with '{' or valid base64 encoded JSON.", e);
                }
                return new ByteArrayInputStream(jsonContent.getBytes(StandardCharsets.UTF_8));
            } catch (Exception e) {
                log.error("Unexpected error processing credentials JSON: {}", e.getMessage(), e);
                throw new IOException("Failed to process credentials JSON: " + e.getMessage(), e);
            }
        }
        
        // Priority 2: Use credentialsFile (for local development)
        if (StringUtils.hasText(properties.getCredentialsFile())) {
            log.info("Attempting to load credentials from file: {}", properties.getCredentialsFile());
            Path credentialsPath = Path.of(properties.getCredentialsFile());
            if (!Files.exists(credentialsPath)) {
                log.error("Google Calendar credentials file not found: {}", properties.getCredentialsFile());
                throw new IllegalStateException("Google Calendar credentials file not found: " + properties.getCredentialsFile());
            }
            log.info("Using credentials file: {}", properties.getCredentialsFile());
            return Files.newInputStream(credentialsPath);
        }
        
        log.error("No Google Calendar credentials configured. Set GOOGLE_CALENDAR_CREDENTIALS_JSON environment variable or GOOGLE_CALENDAR_CREDENTIALS_FILE property.");
        throw new IllegalStateException("No Google Calendar credentials configured. Please set GOOGLE_CALENDAR_CREDENTIALS_JSON environment variable or GOOGLE_CALENDAR_CREDENTIALS_FILE property.");
    }

    private ObjectNode buildEventPayload(Deal deal, LocalDate eventDate) {
        if (eventDate == null) {
            throw new IllegalStateException("No event date provided for deal " + deal.getId());
        }
        ObjectNode root = objectMapper.createObjectNode();
        root.put("summary", buildSummary(deal, eventDate));
        root.put("description", buildDescription(deal, eventDate));
        if (StringUtils.hasText(deal.getVenue())) {
            root.put("location", deal.getVenue());
        }
        ObjectNode start = root.putObject("start");
        start.put("date", eventDate.toString());
        ObjectNode end = root.putObject("end");
        end.put("date", eventDate.plusDays(1).toString());
        return root;
    }
    
    private String buildSummary(Deal deal, LocalDate eventDate) {
        StringBuilder summary = new StringBuilder(deal.getName());
        if (deal.getOrganization() != null && StringUtils.hasText(deal.getOrganization().getName())) {
            summary.append(" • ").append(deal.getOrganization().getName());
        }
        // If there are multiple dates, include the date in the summary
        List<LocalDate> allDates = getAllEventDates(deal);
        if (allDates != null && allDates.size() > 1) {
            summary.append(" (").append(eventDate.toString()).append(")");
        }
        return summary.toString();
    }


    private String buildDescription(Deal deal, LocalDate eventDate) {
        StringBuilder description = new StringBuilder();
        String eventTypeForDate = getEventTypeForDate(deal, eventDate);
        if (StringUtils.hasText(eventTypeForDate)) {
            description.append("Event Type: ").append(eventTypeForDate).append("\n");
        }
        if (deal.getValue() != null) {
            description.append("Deal Value: ").append(deal.getValue()).append("\n");
        }
        if (deal.getPerson() != null && StringUtils.hasText(deal.getPerson().getName())) {
            description.append("Contact: ").append(deal.getPerson().getName()).append("\n");
        }
        if (StringUtils.hasText(deal.getPhoneNumber())) {
            description.append("Phone: ").append(deal.getPhoneNumber()).append("\n");
        }
        if (deal.getStatus() != null) {
            description.append("Status: ").append(deal.getStatus().name()).append("\n");
        }
        description.append("Deal ID: ").append(deal.getId());
        return description.toString();
    }

    /**
     * Resolves the event type for a specific date.
     * Prefers the per-date mapping from eventDateTypes JSON and falls back to deal-level eventType.
     */
    private String getEventTypeForDate(Deal deal, LocalDate eventDate) {
        if (deal == null || eventDate == null) {
            return null;
        }

        // Try per-date mapping first
        if (deal.getEventDateTypes() != null && !deal.getEventDateTypes().isEmpty()) {
            try {
                Map<String, String> map = objectMapper.readValue(
                    deal.getEventDateTypes(),
                    new TypeReference<Map<String, String>>() {}
                );
                String byDate = map.get(eventDate.toString());
                if (StringUtils.hasText(byDate)) {
                    return byDate;
                }
            } catch (Exception e) {
                // Ignore parse errors and fall back to deal-level eventType
            }
        }

        // Fallback: deal-level eventType
        return deal.getEventType();
    }

    private String encode(String value) {
        return URLEncoder.encode(value, StandardCharsets.UTF_8);
    }

    private synchronized String fetchAccessToken() {
        try {
            return credentials.refreshAccessToken().getTokenValue();
        } catch (IOException e) {
            throw new IllegalStateException("Unable to refresh Google access token", e);
        }
    }

    private GoogleCalendarEventPayload toPayload(String calendarId, JsonNode item) {
        if (item == null || !item.hasNonNull("id")) {
            return null;
        }
        String status = item.hasNonNull("status") ? item.get("status").asText() : "confirmed";
        JsonNode startNode = item.get("start");
        JsonNode endNode = item.get("end");
        Instant start = parseInstant(startNode);
        Instant end = parseInstant(endNode);
        boolean allDay = startNode != null && startNode.hasNonNull("date") && !startNode.has("dateTime");
        String summary = item.hasNonNull("summary") ? item.get("summary").asText() : null;
        String description = item.hasNonNull("description") ? item.get("description").asText() : null;
        if (start == null || end == null) {
            return null;
        }
        return new GoogleCalendarEventPayload(
                calendarId,
                item.get("id").asText(),
                status,
                allDay,
                start,
                end,
                summary,
                description
        );
    }

    private Instant parseInstant(JsonNode node) {
        if (node == null) {
            return null;
        }
        if (node.hasNonNull("dateTime")) {
            return Instant.parse(node.get("dateTime").asText());
        }
        if (node.hasNonNull("date")) {
            return LocalDate.parse(node.get("date").asText()).atStartOfDay(ZoneOffset.UTC).toInstant();
        }
        return null;
    }

    public record GoogleCalendarEventPayload(
            String calendarId,
            String eventId,
            String status,
            boolean allDay,
            Instant startAt,
            Instant endAt,
            String summary,
            String description
    ) {
    }
}


