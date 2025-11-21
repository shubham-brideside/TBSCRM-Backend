# Calendar Integration Documentation

This document describes the calendar integration implementation that was copied from `calendar backend` to `TBSCRM-Backend`.

## Overview

The calendar integration enables automatic synchronization between the CRM system and Google Calendar. It supports:
- **CRM → Google Calendar**: Automatically creates/updates/deletes calendar events when deals are created, updated, or deleted
- **Google Calendar → CRM**: Polls vendor calendars to mirror external calendar changes into the CRM database

## Files Modified

### DealServiceImpl.java
**Location**: `src/main/java/com/brideside/crm/service/impl/DealServiceImpl.java`

**Changes Made**:
1. Added import for `GoogleCalendarService`
2. Added `@Autowired(required = false) GoogleCalendarService` field dependency
3. Added `StringUtils` import for calendar sync utility methods
4. Added `syncGoogleCalendarEvent()` call in `create()` method (after deal is saved)
5. Added `syncGoogleCalendarEvent()` call in `markStatus()` method (already existed, now properly implemented)
6. Added `removeGoogleCalendarEvent()` call in `delete()` method (both hard delete and soft delete versions)
7. Implemented `syncGoogleCalendarEvent()` private method
8. Implemented `removeGoogleCalendarEvent()` private method

**Key Methods Added**:

```java
private void syncGoogleCalendarEvent(Deal deal)
```
- Syncs a deal to Google Calendar if:
  - The deal has an `eventDate`
  - The deal's organization has a `googleCalendarId`
- Creates a new calendar event or updates an existing one
- Stores the `googleCalendarEventId` on the deal for future updates

```java
private void removeGoogleCalendarEvent(Deal deal)
```
- Removes the associated Google Calendar event when a deal is deleted
- Clears the `googleCalendarEventId` from the deal record

## Existing Calendar Files

The following calendar-related files were already present and identical in both directories:

### Controllers
- `CalendarController.java` - REST API endpoint for vendor calendar events (`/api/calendar/vendor-events`)

### Entities
- `VendorCalendarEvent.java` - Entity representing vendor calendar events mirrored from Google Calendar

### Services
- `GoogleCalendarService.java` - Service for interacting with Google Calendar API
- `VendorCalendarSyncService.java` - Interface for vendor calendar synchronization
- `VendorCalendarSyncServiceImpl.java` - Implementation that polls vendor calendars periodically

### Repositories
- `VendorCalendarEventRepository.java` - Repository for vendor calendar events

### DTOs
- `CalendarDtos.java` - Data transfer objects for calendar responses

### Configuration
- `GoogleCalendarProperties.java` - Configuration properties for Google Calendar integration

### Database Migrations
- `V20251119_01__organizations_google_calendar.sql` - Adds `google_calendar_id` to organizations and `google_calendar_event_id` to deals
- `V20251119_02__vendor_calendar_events.sql` - Creates `vendor_calendar_events` table

### Application Configuration
- `application.yml` - Contains Google Calendar configuration under `google.calendar` section

## Configuration

The calendar integration is configured in `application.yml`:

```yaml
google:
  calendar:
    enabled: ${GOOGLE_CALENDAR_ENABLED:true}
    application-name: ${GOOGLE_CALENDAR_APP_NAME:Brideside CRM}
    credentials-file: ${GOOGLE_CALENDAR_CREDENTIALS_FILE:}
    credentials-json: ${GOOGLE_CALENDAR_CREDENTIALS_JSON:}
    impersonated-user: ${GOOGLE_CALENDAR_IMPERSONATED_USER:}
    poll-interval-minutes: ${GOOGLE_CALENDAR_POLL_MINUTES:10}
    sync-window-past-days: ${GOOGLE_CALENDAR_SYNC_PAST_DAYS:30}
    sync-window-future-days: ${GOOGLE_CALENDAR_SYNC_FUTURE_DAYS:120}
```

## How It Works

### Deal to Calendar Sync (CRM → Google)

1. **When a deal is created** with an `eventDate` and the organization has a `googleCalendarId`:
   - `syncGoogleCalendarEvent()` is called automatically
   - An all-day event is created in the vendor's Google Calendar
   - The event includes deal name, organization, event type, venue, contact info, and deal value
   - The `googleCalendarEventId` is stored on the deal

2. **When a deal status changes** (e.g., marked as WON):
   - `syncGoogleCalendarEvent()` is called to update the calendar event
   - The calendar event description is updated with the new status

3. **When a deal is deleted**:
   - `removeGoogleCalendarEvent()` is called
   - The associated Google Calendar event is deleted
   - The `googleCalendarEventId` is cleared from the deal

### Calendar to CRM Sync (Google → CRM)

1. **Scheduled Polling**:
   - `VendorCalendarSyncServiceImpl` runs every 10 minutes (configurable)
   - Polls all organizations that have a `googleCalendarId`
   - Fetches events from a time window (30 days past, 120 days future by default)

2. **Event Mirroring**:
   - Events found in Google Calendar are stored in `vendor_calendar_events` table
   - Updates overwrite existing records
   - Cancelled events are removed from the database
   - Events that no longer exist in Google Calendar are removed from the database

3. **API Access**:
   - Vendor calendar events are exposed via `GET /api/calendar/vendor-events`
   - Can be filtered by `organizationId`, `from`, and `to` query parameters

## API Endpoints

### Vendor Calendar Events
- **GET** `/api/calendar/vendor-events`
- **Query Parameters**:
  - `organizationId` (optional): Filter by organization
  - `from` (optional): Start date/time (ISO format)
  - `to` (optional): End date/time (ISO format)
- **Response**: List of vendor calendar events with details

## Dependencies

The calendar integration requires:
- Google Calendar API credentials (service account JSON file or JSON string)
- Google Calendar API access enabled for the service account
- Organizations configured with `googleCalendarId` (email of the calendar)

## Notes

- The `GoogleCalendarService` is conditionally enabled based on `google.calendar.enabled` property
- Calendar sync methods gracefully handle failures and log warnings
- The integration uses `@Autowired(required = false)` to allow the application to start even if Google Calendar is not configured
- All calendar operations are non-blocking and won't prevent deal operations from completing

## Testing

To test the calendar integration:

1. Ensure Google Calendar is enabled in `application.yml`
2. Configure a service account with Google Calendar API access
3. Create an organization with a `googleCalendarId`
4. Create a deal with an `eventDate` for that organization
5. Verify the event appears in the Google Calendar
6. Check `/api/calendar/vendor-events` to see mirrored vendor calendar events

## Date Completed

Calendar integration copied from `calendar backend` to `TBSCRM-Backend` on the current date.

