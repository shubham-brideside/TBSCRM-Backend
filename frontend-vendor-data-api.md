# Vendor Data API – Frontend Integration Guide

This document describes the APIs for managing **Vendor Data** (URLs) on the Organization Details page (VENDOR DATA section).

---

## Overview

Vendor data stores two URLs per vendor:
- **Master Data Link** – URL for master data
- **Calendar Sheet Link** – URL for calendar sheet

One record per vendor. Use Cancel to discard changes, Save to persist.

---

## Base URL & Auth

**Base URL:** `/api/organizations`

**Auth:** `Authorization: Bearer <token>` on all endpoints

---

## Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/{orgId}/vendors/{vendorId}/vendor-data` | Get vendor data (returns null if none) |
| POST | `/{orgId}/vendors/{vendorId}/vendor-data` | Create vendor data (fails if exists) |
| PUT | `/{orgId}/vendors/{vendorId}/vendor-data` | Save (upsert) – create or update |
| PUT | `/{orgId}/vendors/{vendorId}/vendor-data/{dataId}` | Update by id |
| DELETE | `/{orgId}/vendors/{vendorId}/vendor-data` | Delete vendor data |

**Recommended for Save button:** `PUT /{orgId}/vendors/{vendorId}/vendor-data` (upsert)

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
GET returns `data: null` when no vendor data exists.

---

## TypeScript Types

```ts
interface VendorData {
  id: number;
  vendorId: number;
  masterDataLink: string | null;
  calendarSheetLink: string | null;
  createdAt: string;
  updatedAt: string;
}

interface VendorDataSaveRequest {
  masterDataLink?: string | null;
  calendarSheetLink?: string | null;
}
```

---

## API Details

### 1. Get Vendor Data

**GET** `/{orgId}/vendors/{vendorId}/vendor-data`

**Response:** `data` is `VendorData` or `null` if no record exists.

**Example response (existing):**
```json
{
  "success": true,
  "message": "Vendor data fetched",
  "data": {
    "id": 1,
    "vendorId": 10,
    "masterDataLink": "https://docs.google.com/spreadsheets/d/abc123",
    "calendarSheetLink": "https://calendar.google.com/calendar/embed?src=xyz",
    "createdAt": "2026-03-07T10:00:00",
    "updatedAt": "2026-03-07T10:00:00"
  }
}
```

**Example response (none):**
```json
{
  "success": true,
  "message": "Vendor data fetched",
  "data": null
}
```

---

### 2. Save Vendor Data (Upsert) – Recommended for Save

**PUT** `/{orgId}/vendors/{vendorId}/vendor-data`

Creates or updates vendor data. Use this for the Save button.

**Request body:**
```json
{
  "masterDataLink": "https://docs.google.com/spreadsheets/d/abc123",
  "calendarSheetLink": "https://calendar.google.com/calendar/embed?src=xyz"
}
```

| Field | Type | Required | Notes |
|-------|------|----------|-------|
| masterDataLink | string | No | URL |
| calendarSheetLink | string | No | URL |

**Response:** `data` is the saved `VendorData`.

---

### 3. Create Vendor Data

**POST** `/{orgId}/vendors/{vendorId}/vendor-data`

Creates vendor data. Fails with 400 if data already exists for this vendor.

**Request body:** Same as PUT save.

---

### 4. Update Vendor Data (by id)

**PUT** `/{orgId}/vendors/{vendorId}/vendor-data/{dataId}`

Updates an existing record by id.

**Request body:** Same as save.

---

### 5. Delete Vendor Data

**DELETE** `/{orgId}/vendors/{vendorId}/vendor-data`

Removes vendor data for the vendor. No request body.

**Response:** `data` is null. HTTP 200 on success.

---

## API Client (TypeScript)

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

  const json = await res.json();
  if (!res.ok || !json.success) {
    throw new Error(json.message || "Request failed");
  }
  return json.data;
}

// Vendor data
export const getVendorData = (orgId: number, vendorId: number) =>
  api<VendorData | null>(`/${orgId}/vendors/${vendorId}/vendor-data`);

export const saveVendorData = (
  orgId: number,
  vendorId: number,
  body: VendorDataSaveRequest
) =>
  api<VendorData>(`/${orgId}/vendors/${vendorId}/vendor-data`, {
    method: "PUT",
    body,
  });

export const createVendorData = (
  orgId: number,
  vendorId: number,
  body: VendorDataSaveRequest
) =>
  api<VendorData>(`/${orgId}/vendors/${vendorId}/vendor-data`, {
    method: "POST",
    body,
  });

export const updateVendorData = (
  orgId: number,
  vendorId: number,
  dataId: number,
  body: VendorDataSaveRequest
) =>
  api<VendorData>(`/${orgId}/vendors/${vendorId}/vendor-data/${dataId}`, {
    method: "PUT",
    body,
  });

export const deleteVendorData = (orgId: number, vendorId: number) =>
  api<void>(`/${orgId}/vendors/${vendorId}/vendor-data`, {
    method: "DELETE",
  });
```

---

## UI Integration Flow

### VENDOR DATA section

1. **Initial load:** Call `GET /{orgId}/vendors/{vendorId}/vendor-data`. If `data` is null, show empty inputs.
2. **Save:** On Save click, call `PUT /{orgId}/vendors/{vendorId}/vendor-data` with `{ masterDataLink, calendarSheetLink }`.
3. **Cancel:** Discard local edits without calling the API.
4. **Delete (optional):** If you add a "Clear" action, call `DELETE /{orgId}/vendors/{vendorId}/vendor-data`.

### Field mapping

| UI Field | API field |
|----------|-----------|
| Master Data Link | `masterDataLink` |
| Calendar Sheet Link | `calendarSheetLink` |

---

## Error Handling

| HTTP | Meaning |
|------|---------|
| 404 | Organization or vendor not found |
| 401 | Unauthorized – invalid or missing token |
| 403 | Forbidden – insufficient permissions |
| 400 | Bad request – validation error, or create when data already exists |

Always check `res.ok` and `body.success` before using `body.data`.

---

## Related Docs

- `frontend-api-integration-guide.md` – Full Organization Details page integration
- `frontend-brideside-vendors-api.md` – Vendor API details
