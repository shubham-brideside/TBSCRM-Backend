## Calendar Integration Cheat Sheet

Use this document when wiring the frontend to the Google Calendar-aware backend. It summarizes what changed in the data model plus which endpoints need to be called for organizations and deals so that vendor calendars stay in sync.

---

### 1. What Changed
- `Organization` objects now contain a nullable `googleCalendarId` (the vendor’s Google Calendar email).
- `Deal` objects now track `googleCalendarEventId` (managed by the backend) and still rely on `eventDate` to determine when to sync to the vendor calendar.
- When Google Calendar sync is enabled server-side, deals that belong to an organization with `googleCalendarId` and have an `eventDate` will automatically create/update/delete all‑day events.

---

### 2. Organization Endpoints

#### Create Organization
- **POST** `/api/organizations`
- **Body snippet**
  ```json
  {
    "name": "Vendor Name",
    "category": "PHOTOGRAPHY",
    "ownerId": 12,
    "address": "Optional",
    "googleCalendarId": "vendor.calendar@example.com" // optional, email
  }
  ```
- **Response** includes the stored `googleCalendarId`.

#### Update Organization
- **PUT** `/api/organizations/{id}`
- Send the full payload (same shape as create) with the updated `googleCalendarId`.

#### Get/List Organizations
- **GET** `/api/organizations` (list) or `/api/organizations/{id}` (single)
- Responses now include `googleCalendarId` so you can display/edit it.

**Frontend guidance**
- Treat `googleCalendarId` as optional but validate using email pattern before sending.
- Clearing the field (empty string) removes calendar syncing for that vendor.

---

### 3. Deal Endpoints

#### Create Deal
- **POST** `/api/deals`
- **Body fields relevant to calendar**
  ```json
  {
    "name": "Sample Deal",
    "organizationId": 5,      // must belong to org with googleCalendarId to sync
    "eventDate": "2025-12-12",
    "eventType": "Wedding",   // optional, shows up in calendar description
    "venue": "The Plaza",     // optional, becomes event location
    "phoneNumber": "+1-555-1234" // optional, shown in description
  }
  ```
- No need to send `googleCalendarEventId`; backend manages it.

#### Update Deal Stage or Status
- **PUT** `/api/deals/{id}/stage`
- **PATCH** `/api/deals/{id}/status`
- Backend re-syncs the calendar entry whenever status changes (e.g., WON/LOST) or when `eventDate` changes via a future update endpoint.

#### Retrieve Deals
- Any `GET` variant (list, listByOrganization, etc.) returns `eventDate` plus the backend-managed `googleCalendarEventId`. The frontend usually ignores the event id, but it can be displayed for troubleshooting.

#### Delete Deal
- **DELETE** `/api/deals/{id}`
- Backend removes the linked Google Calendar event automatically.

**Frontend guidance**
- Ensure the deal form enforces an ISO date string (`YYYY-MM-DD`) for `eventDate`.
- Encourage users to select an organization that already has a `googleCalendarId`. Without it the calendar will not populate.

---

### 4. Vendor Calendar Polling → CRM Events

The backend now polls each vendor calendar (all organizations with a `googleCalendarId`) every few minutes (configurable, default 10). It uses Google’s `GET /calendar/v3/calendars/{calendarId}/events` API to pull a rolling window of events (30 days past, 120 days future). Any event that appears in Google but is not already in the CRM database is inserted, updates overwrite our copy, and removed/cancelled events are deleted locally. This means changes vendors make directly inside Google Calendar show up on the CRM calendar view without manual action.

#### Vendor events endpoint
- **GET** `/api/calendar/vendor-events`
- **Query params**
  - `organizationId` *(optional)* – scope to a specific vendor
  - `from` / `to` *(optional ISO-8601 instants)* – override default sync window
- **Sample response**
  ```json
  {
    "success": true,
    "message": "Vendor calendar events fetched",
    "data": [
      {
        "id": 42,
        "organizationId": 5,
        "organizationName": "TKB Studios",
        "googleEventId": "3q7s4s6k2",
        "summary": "Anshika – Bridal Trial",
        "description": "Imported from vendor calendar",
        "startAt": "2025-11-14T00:00:00Z",
        "endAt": "2025-11-15T00:00:00Z",
        "allDay": true,
        "status": "confirmed",
        "lastSyncedAt": "2025-11-19T11:05:00Z"
      }
    ]
  }
  ```

**Frontend guidance**
- Call this endpoint alongside your existing deal fetch and merge both datasets in the UI (tag vendor events differently, e.g., color or icon).
- When “Show only organizations with Google Calendar” is toggled, only display deals/events where `organizationId` belongs to such vendors; this endpoint already limits to those orgs.

---

### 4. Calendar Configuration (FYI)
Although the frontend does not manage configuration, it helps to know the ops-level requirements:

```yaml
google:
  calendar:
    enabled: true
    application-name: Brideside CRM
    credentials-file: /path/to/service-account.json   # or credentials-json
    impersonated-user: vendor-admin@example.com       # optional
```

- Ops must share each vendor’s Google Calendar with the service account email so the backend can write events.
- If calendar sync is disabled server-side, `googleCalendarId` is simply stored and unused.

---

### 5. UX Suggestions
- When editing an organization, highlight that `googleCalendarId` should be the vendor calendar’s primary email (not just any contact email).
- On the deal creation form, show a badge or tooltip when the selected organization has calendar sync enabled so the user knows an event will appear automatically.
- Surface backend messages if a deal fails to create; the calendar integration itself never requires extra frontend calls.

---

Keep this file open in Cursor while wiring your forms—it contains everything needed to submit the right payloads and display the new fields. If additional endpoints are introduced (e.g., manual resync), append them here.

