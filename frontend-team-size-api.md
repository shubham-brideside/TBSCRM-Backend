# Team Size Table API – Frontend Integration Guide

This document describes the APIs for managing the **Team Size** table on the Organization Details page (TEAM SIZE section in your screenshot).

---

## Overview

The Team Size table is stored per vendor and includes one row per **guest-count / event-type** combination.

Each row has:
- **Guest count** – Text such as `"0-100"`, `"100-200"`, `"More than 300"`.
- **Event type** – Text such as `"Small Events"`, `"Wedding & Engagement"`, `"All"`.
- **Photographer** – Count or notes (free text).
- **Cinematographer** – Count or notes (free text).
- **Drone** – Count or notes (free text).
- **Notes / Policy** – Free-form notes or policy text.

Example rows (matching your default table):

| guestCount   | eventType            | photographer | cinematographer | drone | notes |
|--------------|----------------------|--------------|-----------------|-------|-------|
| 0-100        | Small Events         | (empty)      | (empty)         | (empty) | (empty) |
| 0-100        | Wedding & Engagement | (empty)      | (empty)         | (empty) | (empty) |
| 100-200      | All                  | (empty)      | (empty)         | (empty) | (empty) |
| 200-300      | All                  | (empty)      | (empty)         | (empty) | (empty) |
| More than 300| All                  | (empty)      | (empty)         | (empty) | (empty) |

---

## Base URL & Auth

**Base URL:** `/api/organizations`

**Auth:** `Authorization: Bearer <token>` on all endpoints

---

## Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/{orgId}/vendors/{vendorId}/team-size` | Get all team size rows for a vendor |
| PUT | `/{orgId}/vendors/{vendorId}/team-size` | Save (replace) all team size rows for a vendor |

Notes:
- There is **no** per-row POST/PUT/DELETE; the table is saved as a **whole list** on each Save click.
- Backend validates that the vendor belongs to the given organization.

---

## Response Envelope

All responses use this structure:

```json
{
  "success": true,
  "message": "Human readable message",
  "data": { ... }
}
```

On error: `success: false`, `message` contains the error description.

---

## TypeScript Types

```ts
// One row in the Team Size table
export interface TeamSizeRow {
  id?: number;          // Present when loaded from backend; omit for new rows
  guestCount: string;   // e.g. "0-100", "More than 300"
  eventType: string;    // e.g. "Small Events", "Wedding & Engagement", "All"
  photographer?: string;
  cinematographer?: string;
  drone?: string;
  notes?: string;
}
```

Backend wrapper types:

```ts
// GET /team-size response data
interface TeamSizeRowsResponse {
  rows: TeamSizeRow[];
}

// PUT /team-size request body
interface TeamSizeSaveRequest {
  rows: TeamSizeRow[];
}
```

---

## API Details

### 1. Get Team Size rows

**GET** `/{orgId}/vendors/{vendorId}/team-size`

**Response:** `data` is a `TeamSizeRowsResponse`:

```json
{
  "success": true,
  "message": "Vendor team size rows fetched",
  "data": {
    "rows": [
      {
        "id": 1,
        "vendorId": 10,
        "guestCount": "0-100",
        "eventType": "Small Events",
        "photographer": "1",
        "cinematographer": "",
        "drone": "",
        "notes": "",
        "createdAt": "2026-03-18T10:00:00",
        "updatedAt": "2026-03-18T10:00:00"
      }
    ]
  }
}
```

If no rows exist for the vendor, `rows` will be an empty array. The frontend can then initialize the default five rows locally.

---

### 2. Save Team Size rows

**PUT** `/{orgId}/vendors/{vendorId}/team-size`

**Request body:**

```json
{
  "rows": [
    {
      "guestCount": "0-100",
      "eventType": "Small Events",
      "photographer": "1",
      "cinematographer": "",
      "drone": "",
      "notes": ""
    },
    {
      "guestCount": "0-100",
      "eventType": "Wedding & Engagement",
      "photographer": "2",
      "cinematographer": "1",
      "drone": "",
      "notes": "Extra charges for late-night events"
    }
  ]
}
```

**Field rules (per row):**

| Field           | Type   | Required | Max length | Notes |
|----------------|--------|----------|-----------|-------|
| `id`           | number | No       | —         | Include when updating an existing row; omit for new rows. |
| `guestCount`   | string | Yes      | 100       | Non-empty. Free text like `"0-100"` or `"More than 300"`. |
| `eventType`    | string | Yes      | 100       | Non-empty. Free text like `"Small Events"`, `"Wedding & Engagement"`, `"All"`. |
| `photographer` | string | No       | 500       | Optional; counts or notes. Empty string allowed. |
| `cinematographer` | string | No    | 500       | Optional; counts or notes. Empty string allowed. |
| `drone`        | string | No       | 500       | Optional; counts or notes. Empty string allowed. |
| `notes`        | string | No       | 1000      | Optional; free-form notes/policy. |

**Behavior:**

- The backend treats the request as **full replacement**:
  - Existing rows for the vendor are deleted.
  - New rows are created from the `rows` array (re-using `id`s is supported but not required).
- If `rows` is empty or missing, all existing rows for the vendor are removed.
- On success, response `data` is a fresh `TeamSizeRowsResponse`:

```json
{
  "success": true,
  "message": "Vendor team size rows saved",
  "data": {
    "rows": [
      {
        "id": 1,
        "vendorId": 10,
        "guestCount": "0-100",
        "eventType": "Small Events",
        "photographer": "1",
        "cinematographer": "",
        "drone": "",
        "notes": "",
        "createdAt": "2026-03-18T10:00:00",
        "updatedAt": "2026-03-18T10:01:00"
      }
    ]
  }
}
```

---

## API Client (TypeScript)

You can extend the existing `api` helper from `frontend-api-integration-guide.md`:

```ts
const API_BASE = "/api/organizations";

async function api<T>(
  path: string,
  options: { method?: string; body?: object } = {}
): Promise<T> {
  const { method = "GET", body } = options;
  const res = await fetch(`${API_BASE}${path}`, {
    method,
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${getToken()}`,
    },
    ...(body && { body: JSON.stringify(body) }),
  });

  const json: { success: boolean; message: string; data: T } = await res.json();
  if (!res.ok || !json.success) {
    throw new Error(json.message || "Request failed");
  }
  return json.data;
}

// Team Size table
export const getTeamSizeRows = (orgId: number, vendorId: number) =>
  api<TeamSizeRowsResponse>(`/${orgId}/vendors/${vendorId}/team-size`);

export const saveTeamSizeRows = (
  orgId: number,
  vendorId: number,
  rows: TeamSizeRow[]
) =>
  api<TeamSizeRowsResponse>(`/${orgId}/vendors/${vendorId}/team-size`, {
    method: "PUT",
    body: { rows },
  });
```

---

## UI Integration Flow (Team Size section)

### 1. Initial load

1. Use `getOrganizationWithDetails(orgId)` to get `organization` + `vendors`.
2. Determine `vendorId` as `vendors[0].id` (current pattern).
3. Call `getTeamSizeRows(orgId, vendorId)`:
   - If `rows.length === 0`, initialize your local table with the five default rows from the spec.
   - Otherwise, render the returned rows directly.

### 2. Edit flow

- When the user clicks **Edit** in the Team Size section:
  - Unlock the table for editing (inline cells or modal).
  - Let them add/remove rows or change values for:
    - Guest count
    - Event type
    - Photographer
    - Cinematographer
    - Drone
    - Notes / Policy

### 3. Save

- On **Save**:
  - Build an array of `TeamSizeRow` from your local state.
  - Call `saveTeamSizeRows(orgId, vendorId, rows)`.
  - Replace your local state with `response.rows` on success.

### 4. Cancel

- On **Cancel**:
  - Discard changes and reload from the backend:
    - Call `getTeamSizeRows` again, or
    - Revert to the snapshot you took when entering edit mode.

### 5. Field mapping (UI ↔ API)

| UI Column         | API field        |
|-------------------|------------------|
| Guest count       | `guestCount`     |
| Event type        | `eventType`      |
| Photographer      | `photographer`   |
| Cinematographer   | `cinematographer`|
| Drone             | `drone`          |
| Notes / Policy    | `notes`          |

---

## Error Handling

| HTTP | Meaning |
|------|---------|
| 404 | Organization or vendor not found |
| 401 | Unauthorized – invalid or missing token |
| 403 | Forbidden – insufficient permissions |
| 400 | Bad request – validation error (e.g. missing guestCount or eventType, string too long) |

Always check `res.ok` and `body.success` before using `body.data`. Show `body.message` in a toast/snackbar when available.

---

## Related Docs

- `frontend-api-integration-guide.md` – Full Organization Details page integration
- `frontend-brideside-vendors-api.md` – Vendor API details
- `frontend-team-members-api.md` – Vendor Team Members section

