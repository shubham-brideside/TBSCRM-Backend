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
    "eventDate": "2025-12-19",
    "label": "DIRECT",
    "source": "Instagram",
    "referencedDealId": null
  }
  ```
  - `name` (required, text).  
  - `value` (optional, decimal). Defaults to `0` if omitted.  
  - `personId`, `pipelineId`, `stageId`, `sourceId`, `organizationId`, `categoryId` are optional but IDs must exist when provided—backend returns 404 otherwise.  
  - `status` (optional) — enum `IN_PROGRESS | WON | LOST`; default is `IN_PROGRESS`.  
  - `commissionAmount` (optional) — overrides auto commission calculation (otherwise derived from selected source).  
  - `eventDate` expects ISO `yyyy-MM-dd`. Boolean flags default to null when omitted.
  - `label` (optional) — enum: `DIRECT`, `DIVERT`, `DESTINATION`, `PARTY MAKEUP`, `PRE WEDDING`. Accepts both formats (e.g., "PARTY MAKEUP" or "PARTY_MAKEUP").
  - `source` (optional) — enum: `Instagram`, `Whatsapp`, `Email`, `Reference`, `Call`, `Website`. Case-insensitive (e.g., "instagram", "Instagram", "INSTAGRAM" all work).
  - `referencedDealId` (optional) — ID of the original deal when creating a diverted deal. **Required when `label` is `DIVERT`**.

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
    "eventDate": "2025-12-19",
    "label": "DIRECT",
    "source": "Instagram",
    "isDiverted": false,
    "referencedDealId": null,
    "referencedPipelineId": null
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

## Deal Diversion

### Get Available Pipelines for Diversion

When diverting a deal, you need to show only pipelines where the deal hasn't been diverted yet. Use this endpoint to get the list of available pipelines.

- **Method / URL:** `GET /api/deals/{dealId}/available-pipelines`
- **Description:** Returns pipelines where the specified deal has not been diverted yet. Use this when showing pipeline options in the diversion flow.
- **Response (200 OK):** Array of `PipelineResponse` objects
  ```json
  [
    {
      "id": 5,
      "name": "Photography Pipeline",
      "category": "PHOTOGRAPHY",
      "teamId": 2,
      "isDeleted": false,
      "createdAt": "2025-11-10T10:00:00Z",
      "updatedAt": "2025-11-10T10:00:00Z"
    },
    {
      "id": 8,
      "name": "Makeup Pipeline",
      "category": "MAKEUP",
      "teamId": 3,
      "isDeleted": false,
      "createdAt": "2025-11-10T10:00:00Z",
      "updatedAt": "2025-11-10T10:00:00Z"
    }
  ]
  ```
- **Notes:**
  - Only returns active (non-deleted) pipelines
  - Excludes pipelines where a diverted deal already exists for the specified deal
  - Excludes the referenced pipeline (the original pipeline from which the deal was diverted, if applicable)
  - If the deal doesn't exist, returns `404 Not Found`

### Creating a Diverted Deal

To create a diverted deal:

1. **Get available pipelines** using `GET /api/deals/{originalDealId}/available-pipelines`
2. **Create the new deal** with:
   - `label`: `"DIVERT"` (required)
   - `referencedDealId`: ID of the original deal (required)
   - `pipelineId`: One of the available pipelines from step 1
   - Other deal fields as needed

**Example Request:**
```json
{
  "name": "Diverted Deal - Photography",
  "value": 0,
  "label": "DIVERT",
  "referencedDealId": 123,
  "pipelineId": 5,
  "stageId": 20,
  "personId": 101,
  "status": "IN_PROGRESS"
}
```

**Note:** The `value` field is automatically set to `0` for diverted deals, regardless of what is sent in the request. You can omit the `value` field or send `0`/`null` - the backend will handle it.

**What happens:**
- The backend automatically sets `isDiverted = true` when `label` is `DIVERT`
- The `referencedDealId` links the new deal to the original deal
- The `referencedPipelineId` is automatically set to the **initial/original pipeline** from which the deal was first diverted
  - If diverting an original deal: uses the original deal's current pipeline
  - If diverting an already-diverted deal: uses the original pipeline from the first diversion (traverses up the chain)
- The original deal's ID is stored in the `referenced_deal_id` column
- The original pipeline ID is stored in the `referenced_pipeline_id` column

**Important:** When a diverted deal is diverted again, the `referencedPipelineId` always points to the initial/original pipeline, not the pipeline of the deal being diverted.

**Response includes:**
```json
{
  "id": 215,
  "name": "Diverted Deal - Photography",
  "isDiverted": true,
  "referencedDealId": 123,
  "referencedPipelineId": 8,
  ...
}
```

---

## Field Reference

- `status`: enum defined in backend (`DealStatus`).  
- `commissionAmount`: when omitted the backend uses the associated source's fixed amount or percentage.  
- `eventDate`: persisted as `LocalDate`; send `yyyy-MM-dd`.  
- `phoneNumber` is separate from the linked person's phone—when a person is attached, the backend copies their current phone into `contactNumber`.  
- `finalThankYouSent`, `eventDateAsked`, `contactNumberAsked`, `venueAsked` are booleans; `null` means "not captured yet".
- `label`: enum with values `DIRECT`, `DIVERT`, `DESTINATION`, `PARTY MAKEUP`, `PRE WEDDING`. Accepts both space-separated and underscore formats (e.g., "PARTY MAKEUP" or "PARTY_MAKEUP"). Returns display format with spaces.
- `source`: enum with values `Instagram`, `Whatsapp`, `Email`, `Reference`, `Call`, `Website`. Case-insensitive input (e.g., "instagram", "Instagram", "INSTAGRAM" all accepted). Returns properly capitalized display format.
- `isDiverted`: boolean indicating if this deal was diverted from another deal. Automatically set to `true` when `label` is `DIVERT`.
- `referencedDealId`: ID of the original deal if this is a diverted deal. `null` for non-diverted deals. Required when creating a diverted deal.
- `referencedPipelineId`: ID of the **initial/original pipeline** from which the deal was first diverted. Automatically set when creating a diverted deal. Always points to the original pipeline, even when diverting a diverted deal. Used to prevent diverting back to the original pipeline.

---

## Dropdown Options

### Label Options
The following label values are available for the deal label dropdown:
- `DIRECT`
- `DIVERT`
- `DESTINATION`
- `PARTY MAKEUP` (can also be sent as "PARTY_MAKEUP")
- `PRE WEDDING` (can also be sent as "PRE_WEDDING")

### Source Options
The following source values are available for the deal source dropdown:
- `Instagram`
- `Whatsapp`
- `Email`
- `Reference`
- `Call`
- `Website`

**Note:** Both `label` and `source` are optional fields. If not provided, they will be `null` in the response.

---

## Error Scenarios

- Missing required fields → `400 Bad Request`.  
- Referencing non-existent related IDs → `404 Not Found`.  
- Invalid `status` or malformed date strings → `400 Bad Request`.
- Invalid `label` value → `400 Bad Request` with message: "Invalid label value: {value}. Allowed values: DIRECT, DIVERT, DESTINATION, PARTY MAKEUP, PRE WEDDING".
- Invalid `source` value → `400 Bad Request` with message: "Invalid source value: {value}. Allowed values: Instagram, Whatsapp, Email, Reference, Call, Website".
- Creating a diverted deal without `referencedDealId` → `400 Bad Request` with message: "referencedDealId is required when label is DIVERT".
- Referenced deal not found → `404 Not Found` with message: "Referenced deal not found with id {id}".
- Getting available pipelines for non-existent deal → `404 Not Found` with message: "Referenced deal not found with id {id}".

---

## UI Implementation Guide

### Deal Diversion Flow

1. **User clicks "Divert Deal" on an existing deal**
   - Get the deal ID (e.g., `dealId = 123`)

2. **Fetch available pipelines**
   ```
   GET /api/deals/123/available-pipelines
   ```
   - Show these pipelines in a dropdown
   - If empty array, show message: "This deal has been diverted to all available pipelines"

3. **User selects pipeline and fills deal form**
   - Set `label: "DIVERT"`
   - Set `referencedDealId: 123` (the original deal ID)
   - Set `pipelineId` to the selected pipeline
   - Fill other required fields

4. **Submit the diverted deal**
   ```
   POST /api/deals
   ```
   - Backend automatically sets `isDiverted: true`
   - The new deal is linked to the original via `referencedDealId`

5. **Display diverted deals**
   - Check `isDiverted` field to show diversion indicator
   - Use `referencedDealId` to link back to original deal
   - Filter or group deals by diversion status if needed

### Notes

- A deal can be diverted to multiple pipelines, but only once per pipeline
- The original deal remains unchanged when diverted
- Diverted deals are independent deals with their own stages, status, etc.
- Use `GET /api/deals/{dealId}/available-pipelines` to prevent showing pipelines where the deal is already diverted
- The endpoint automatically excludes the referenced pipeline (original pipeline from which the deal was diverted) from the available options
- If a deal is already diverted, clicking "Divert Deal" will show available pipelines excluding the original pipeline stored in `referencedPipelineId`

---

Hook these endpoints into the deals UI for creating, listing, filtering by status, changing stages/status, and handling deal diversion. Let me know if you need pagination guidance or additional filters. 

