# Deals API Reference (Frontend)

All deal endpoints sit under `POST /api/deals` etc. Use these notes to wire the CRM UI.

---

## Create Deal

- **Method / URL:** `POST /api/deals`
- **Body (`DealDtos.CreateRequest`):**
  ```json
  {
    "name": "Wedding Package - Sharma",
    "value": 125000,
    "personId": 101,
    "pipelineId": 8,
    "stageId": 37,
    "sourceId": 4,
    "organizationId": 12,
    "categoryId": 3,
    "eventType": "Mehendi",
    "status": "IN_PROGRESS",
    "commissionAmount": 7500,
    "venue": "The Taj Palace",
    "phoneNumber": "+91 98765 43210",
    "finalThankYouSent": false,
    "eventDateAsked": true,
    "contactNumberAsked": true,
    "venueAsked": false,
    "eventDate": "2025-12-19"
  }
  ```
  - `name` (required, text).  
  - `value` (optional, decimal). Defaults to `0` if omitted.  
  - `personId`, `pipelineId`, `stageId`, `sourceId`, `organizationId`, `categoryId` are optional but IDs must exist when provided—backend returns 404 otherwise.  
  - `status` (optional) — enum `IN_PROGRESS | WON | LOST`; default is `IN_PROGRESS`.  
  - `commissionAmount` (optional) — overrides auto commission calculation (otherwise derived from selected source).  
  - `eventDate` expects ISO `yyyy-MM-dd`. Boolean flags default to null when omitted.

- **Response (200 OK):** `DealResponse`
  ```json
  {
    "id": 215,
    "name": "Wedding Package - Sharma",
    "value": 125000,
    "personId": 101,
    "pipelineId": 8,
    "stageId": 37,
    "sourceId": 4,
    "organizationId": 12,
    "categoryId": 3,
    "eventType": "Mehendi",
    "status": "IN_PROGRESS",
    "commissionAmount": 7500,
    "createdAt": "2025-11-12T07:05:41.114Z",
    "venue": "The Taj Palace",
    "phoneNumber": "+91 98765 43210",
    "finalThankYouSent": false,
    "eventDateAsked": true,
    "contactNumberAsked": true,
    "venueAsked": false,
    "eventDate": "2025-12-19"
  }
  ```

---

## Fetch Deals

| Endpoint | Description | Notes |
| --- | --- | --- |
| `GET /api/deals` | List all deals | Returns array of `DealResponse`; no pagination currently. |
| `GET /api/deals/{id}` | Fetch single deal | 404 if missing. |
| `GET /api/deals/won` | Deals with status `WON` | |
| `GET /api/deals/lost` | Deals with status `LOST` | |
| `GET /api/deals/inprogress` | Deals with status `IN_PROGRESS` | |
| `GET /api/deals/person/{personId}` | Deals linked to a person | Person must exist. |
| `GET /api/deals/organization/{organizationId}` | Deals for an organization | Organization must exist. |
| `GET /api/deals/category/{categoryId}` | Deals for a category | Category must exist. |

All listings return plain JSON arrays—order is whatever the repository returns (currently natural order). Apply client-side sorting as needed.

---

## Move Deal to Another Stage

- **Method / URL:** `PUT /api/deals/{dealId}/stage`
- **Body:**
  ```json
  {
    "stageId": 42
  }
  ```
- **Response:** Updated `DealResponse`. Stage must belong to the same pipeline (backend does not currently validate association, so ensure UI only offers compatible stages).

---

## Update Deal Status

- **Method / URL:** `PATCH /api/deals/{dealId}/status`
- **Body:**
  ```json
  { "status": "WON" }
  ```
  Allowed values: `IN_PROGRESS`, `WON`, `LOST`.

- **Response:** Updated `DealResponse`. Backend syncs the legacy `won` column and recalculates commission if moving to `WON` and no manual commission provided.

---

## Field Reference

- `status`: enum defined in backend (`DealStatus`).  
- `commissionAmount`: when omitted the backend uses the associated source’s fixed amount or percentage.  
- `eventDate`: persisted as `LocalDate`; send `yyyy-MM-dd`.  
- `phoneNumber` is separate from the linked person’s phone—when a person is attached, the backend copies their current phone into `contactNumber`.  
- `finalThankYouSent`, `eventDateAsked`, `contactNumberAsked`, `venueAsked` are booleans; `null` means “not captured yet”.

---

## Error Scenarios

- Missing required fields → `400 Bad Request`.  
- Referencing non-existent related IDs → `404 Not Found`.  
- Invalid `status` or malformed date strings → `400 Bad Request`.

---

Hook these endpoints into the deals UI for creating, listing, filtering by status, and changing stages/status. Let me know if you need pagination guidance or additional filters. 

