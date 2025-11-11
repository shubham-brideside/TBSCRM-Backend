## Activities API Cheatsheet

Use these endpoints to drive the Activities workspace (tasks, calls, meetings). Every response follows the common envelope:

```json
{
  "success": true|false,
  "message": "Human readable message",
  "data": { ... } // optional
}
```

Validation problems return HTTP `400` with `success: false`. Missing resources return HTTP `404`.

---

### Base URL

- `/api/activities`

---

### List activities

- **Method / URL:** `GET /api/activities`
- **Query params (all optional):**
  - `personId` — filter by linked person id.
  - `dateFrom`, `dateTo` — inclusive range on the `date` field (format `dd/MM/yyyy`).
  - `assignedUser` — case-insensitive contains match on the assigned user email/name.
  - `category` — one of `ACTIVITY`, `CALL`, `MEETING_SCHEDULER`.
  - `status` — one of `OPEN`, `PENDING`, `IN_PROGRESS`, `COMPLETED`, `CANCELLED`.
  - `callType` — one of `INBOUND`, `OUTBOUND`, `MISSED`.
  - `done` — `true`/`false` to show completed or open items.
  - Standard Spring pagination params (`page`, `size`, `sort`). Default: `size=25`, sort by `date`.
- **Response (200 OK):**
  ```json
  {
    "content": [
      {
        "id": 101,
        "subject": "Call bride for venue preferences",
        "category": "CALL",
        "status": "IN_PROGRESS",
        "priority": "HIGH",
        "assignedUser": "sales.rep@brideside.com",
        "date": "08/11/2025",
        "startTime": "11:00",
        "endTime": "11:30",
        "personId": 55,
        "dealId": 21,
        "organization": "Brideside Chicago",
        "dealName": "Jackson Wedding",
        "phone": "+1-555-0100",
        "callType": "OUTBOUND",
        "done": false,
        "createdAt": "2025-11-08T08:00:42Z",
        "updatedAt": "2025-11-08T08:05:13Z"
      }
    ],
    "pageable": { "...": "..." },
    "totalPages": 4,
    "totalElements": 78,
    "last": false,
    "size": 25,
    "number": 0,
    "sort": { "...": "..." },
    "first": true,
    "numberOfElements": 25,
    "empty": false
  }
  ```

---

### Create activity

- **Method / URL:** `POST /api/activities`
- **Body (`ActivityDTO`):**
  ```json
  {
    "subject": "Venue walkthrough follow-up",     // required
    "category": "ACTIVITY",                       // optional, defaults null
    "priority": "MEDIUM",                         // optional
    "status": "OPEN",                             // optional
    "assignedUser": "coordinator@brideside.com",  // optional
    "notes": "Send floor plan pdf",
    "date": "10/11/2025",                         // optional (dd/MM/yyyy)
    "startTime": "15:30",                         // optional (HH:mm)
    "endTime": "16:00",                           // optional
    "dueDate": "11/11/2025",                      // optional
    "personId": 55,                               // optional for now
    "dealId": 21,                                 // optional for now
    "dealName": "Jackson Wedding",                // optional
    "organization": "Brideside Chicago",
    "scheduleBy": "manager@brideside.com",
    "instagramId": "@jacksonweds",
    "phone": "+1-555-0100",
    "callType": "OUTBOUND",                       // only when category=CALL
    "dateTime": "2025-11-10T15:30:00+05:30"       // optional canonical timestamp
  }
  ```
- **Response (201 Created):**
  ```json
  {
    "id": 205,
    "subject": "Venue walkthrough follow-up",
    "category": "ACTIVITY",
    "priority": "MEDIUM",
    "status": "OPEN",
    "assignedUser": "coordinator@brideside.com",
    "notes": "Send floor plan pdf",
    "date": "10/11/2025",
    "startTime": "15:30",
    "endTime": "16:00",
    "dueDate": "11/11/2025",
    "personId": 55,
    "dealId": 21,
    "dealName": "Jackson Wedding",
    "organization": "Brideside Chicago",
    "scheduleBy": "manager@brideside.com",
    "instagramId": "@jacksonweds",
    "phone": "+1-555-0100",
    "callType": "OUTBOUND",
    "done": false,
    "dateTime": "2025-11-10T15:30:00+05:30",
    "createdAt": "2025-11-08T09:00:00Z",
    "updatedAt": "2025-11-08T09:00:00Z"
  }
  ```
- **Notes:**
  - Only `subject` is required today; person/deal/date/time fields remain optional for quick testing.
  - If you provide `personId` or `dealId`, they must exist or the API returns a 400 with a helpful message.

---

### Update activity

- **Method / URL:** `PUT /api/activities/{id}`
- **Body:** Same schema as create; send the properties you want to override.
- **Response (200 OK):** Updated `ActivityDTO`.
- **Notes:** This is a full update (`PUT`), so include the latest values for any fields you want retained.

---

### Toggle done flag

- **Method / URL:** `POST /api/activities/{id}/done?value={true|false}`
- **Behavior:** Sets the `done` flag without needing the full payload.
- **Response (200 OK):** Updated `ActivityDTO` with the new `done` value.

---

### Delete activity

- **Method / URL:** `DELETE /api/activities/{id}`
- **Response:** `204 No Content` on success.
- **Notes:** Hard delete; no soft-delete layer.

---

### Field reference

- **category:** `ACTIVITY`, `CALL`, `MEETING_SCHEDULER`
- **status:** `OPEN`, `PENDING`, `IN_PROGRESS`, `COMPLETED`, `CANCELLED`
- **priority:** `HIGH`, `MEDIUM`, `LOW`
- **callType:** `INBOUND`, `OUTBOUND`, `MISSED` (only relevant when `category=CALL`)
- **date / dueDate:** `dd/MM/yyyy`
- **startTime / endTime:** `HH:mm`
- **dateTime:** ISO timestamp (`yyyy-MM-dd'T'HH:mm[:ss][XXX]`)

---

### Integration tips

1. **Authentication:** Reuse the same JWT bearer token workflow as other APIs.
2. **Filtering:** Source filters from the front-end search UI; blank strings are ignored server-side.
3. **Pagination:** Use `page` (0-based) and `size` to drive infinite scroll or table pagination.
4. **Call-specific fields:** Set `category=CALL` when you want to store `callType`, `phone`, and `scheduleBy`.
5. **Optimistic UI:** After POST/PUT/mark-done, the API returns the latest DTO—merge it back into your client store to avoid refetching immediately.

