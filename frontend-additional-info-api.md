# Additional Info Section API – Frontend Integration Guide

This document describes the APIs for managing the **Additional Info** section on the Organization Details page.

---

## Overview

The Additional Info section appears on the Organization Details page for organizations in these categories:

- `PHOTOGRAPHY`
- `PLANNING`
- `DECOR`
- `PLANNING_AND_DECOR`

It captures vendor-specific details such as pricing, turnaround time, photography style, travel charges, vendor contract, vendor logo, and custom fields.

---

## Base URL & Auth

**Base URL:** `/api/organizations`

**Auth:** `Authorization: Bearer <token>` on all endpoints

---

## Endpoints

| Method | Endpoint                                                                                         | Description                                        |
|--------|--------------------------------------------------------------------------------------------------|----------------------------------------------------|
| GET    | `/{orgId}/vendors/{vendorId}/additional-info`                                                    | Get vendor additional info (fixed + custom fields) |
| PUT    | `/{orgId}/vendors/{vendorId}/additional-info`                                                    | Save vendor additional info                        |
| POST   | `/{orgId}/vendors/{vendorId}/additional-info/vendor-contract/upload` (multipart/form-data)      | Upload vendor contract (PDF or image)             |
| POST   | `/{orgId}/vendors/{vendorId}/additional-info/vendor-logo/upload` (multipart/form-data)          | Upload vendor logo (PDF or image)                 |

Notes:
- Backend validates that the vendor belongs to the given organization.

---

## Response Envelope

All responses use this structure:

```json
{
  "success": true,
  "message": "Human readable message",
  "data": { }
}
```

On error: `success: false`, `message` contains the error description.

---

## TypeScript Types

### Additional Info data

```ts
export type AdditionalCustomFieldType = "number" | "text" | "mixed";

// One custom field (dynamic)
export interface AdditionalCustomField {
  id: number;       // backend-generated
  label: string;
  type: AdditionalCustomFieldType;
  value: string;
}

// Full Additional Info payload from backend
export interface AdditionalInfoData {
  id?: number;
  vendorId?: number;
  startingPriceTwoDayWedding: string | null;
  weddingPerDay: string | null;
  turnaroundTime: string | null;
  photographyStyle: string | null;
  travelAccommodationSeparate: "yes" | "no" | null; // null when empty
  vendorContractUrl: string | null;
  vendorLogoUrl: string | null;
  customFields: AdditionalCustomField[];
  createdAt?: string;
  updatedAt?: string;
}
```

### Save request body

```ts
// PUT /additional-info request body
export interface AdditionalInfoSaveRequest {
  startingPriceTwoDayWedding?: string | null;
  weddingPerDay?: string | null;
  turnaroundTime?: string | null;
  photographyStyle?: string | null;
  travelAccommodationSeparate?: "yes" | "no" | "" | null;
  vendorContractUrl?: string | null;
  vendorLogoUrl?: string | null;
  customFields?: {
    label: string;
    type: AdditionalCustomFieldType;
    value: string;
  }[];
}
```

---

## API Details

### 1. Get Additional Info

**GET** `/{orgId}/vendors/{vendorId}/additional-info`

**Response:** `data` is an `AdditionalInfoData` object or `null` if no record exists yet.

Example success:

```json
{
  "success": true,
  "message": "Vendor additional info fetched",
  "data": {
    "id": 1,
    "vendorId": 10,
    "startingPriceTwoDayWedding": "30k",
    "weddingPerDay": "10 hours",
    "turnaroundTime": "30 days",
    "photographyStyle": "Candid, Traditional",
    "travelAccommodationSeparate": "yes",
    "vendorContractUrl": "https://...",
    "vendorLogoUrl": "https://...",
    "customFields": [
      { "id": 5, "label": "Extra coverage", "type": "text", "value": "Up to 2 hours extra" }
    ],
    "createdAt": "2026-03-18T10:00:00",
    "updatedAt": "2026-03-18T10:10:00"
  }
}
```

If no record exists, `data` will be `null`:

```json
{
  "success": true,
  "message": "Vendor additional info fetched",
  "data": null
}
```

The frontend should treat `null` as “empty form” and initialize defaults locally.

---

### 2. Save Additional Info

**PUT** `/{orgId}/vendors/{vendorId}/additional-info`

**Request body:** `AdditionalInfoSaveRequest`.

Example:

```json
{
  "startingPriceTwoDayWedding": "30k",
  "weddingPerDay": "10 hours",
  "turnaroundTime": "30 days",
  "photographyStyle": "Candid, Traditional",
  "travelAccommodationSeparate": "yes",
  "vendorContractUrl": null,
  "vendorLogoUrl": null,
  "customFields": [
    { "label": "Extra coverage", "type": "text", "value": "Up to 2 hours extra" },
    { "label": "Deliverables", "type": "mixed", "value": "Photo + Video" }
  ]
}
```

**Field rules:**

| Field                        | Type                       | Required | Max length | Notes |
|-----------------------------|----------------------------|----------|-----------|-------|
| `startingPriceTwoDayWedding`| string                     | No       | 100       | Free-form text (e.g. `"5000"`, `"30k"`, `"1L"`) |
| `weddingPerDay`             | string                     | No       | 100       | Free-form per-day text |
| `turnaroundTime`            | string                     | No       | 200       | Free-form duration |
| `photographyStyle`          | string                     | No       | 500       | Free-form styles |
| `travelAccommodationSeparate` | `"yes"`\|`"no"`\|`""`\|null | No     | 10        | Backend normalizes to `"yes"`, `"no"`, or `null` |
| `vendorContractUrl`         | string or null             | No       | 1024      | Normally set by upload API |
| `vendorLogoUrl`             | string or null             | No       | 1024      | Normally set by upload API |
| `customFields[].label`      | string                     | No       | 255       | If empty/blank, field is ignored |
| `customFields[].type`       | `"text"`\|`"number"`\|`"mixed"` | No  | 20        | Must match enum |
| `customFields[].value`      | string                     | No       | 1000      | Free-form value |

**Behavior:**

- If no Additional Info exists yet for the vendor, the backend **creates** one.
- If one exists, the backend **updates** it.
- **Custom fields** are treated as a full replacement:
  - Existing custom fields are removed.
  - New list from `customFields` is persisted.
  - Backend generates new `id`s and returns them in the response.
- On success, response `data` is the fresh `AdditionalInfoData` from the database.

Example success:

```json
{
  "success": true,
  "message": "Vendor additional info saved",
  "data": {
    "id": 1,
    "vendorId": 10,
    "startingPriceTwoDayWedding": "30k",
    "weddingPerDay": "10 hours",
    "turnaroundTime": "30 days",
    "photographyStyle": "Candid, Traditional",
    "travelAccommodationSeparate": "yes",
    "vendorContractUrl": null,
    "vendorLogoUrl": null,
    "customFields": [
      { "id": 7, "label": "Extra coverage", "type": "text", "value": "Up to 2 hours extra" }
    ]
  }
}
```

---

### 3. Upload Vendor Contract

**POST** `/{orgId}/vendors/{vendorId}/additional-info/vendor-contract/upload`

**Content-Type:** `multipart/form-data`  
**Field:** `file` (PDF or image)

**Rules:**

- Allowed content types: `application/pdf`, `image/jpeg`, `image/jpg`, `image/png`, `image/webp`, `image/gif`.
- Max size: 10MB (enforced by backend).
- On success:
  - File is uploaded to Azure Blob Storage.
  - `vendorContractUrl` is updated in Additional Info.
  - Any previous contract blob may be deleted.
- Response `data` is the full updated `AdditionalInfoData`.

**Error (Azure not configured):**

```json
{
  "success": false,
  "message": "Azure Blob Storage is not configured. Set AZURE_STORAGE_BLOB_CONNECTION_STRING."
}
```

---

### 4. Upload Vendor Logo

**POST** `/{orgId}/vendors/{vendorId}/additional-info/vendor-logo/upload`

**Content-Type:** `multipart/form-data`  
**Field:** `file` (PDF or image)

**Rules:**

- Allowed content types: `application/pdf`, `image/jpeg`, `image/jpg`, `image/png`, `image/webp`, `image/gif`.
- Max size: 10MB (enforced by backend).
- On success:
  - File is uploaded to Azure Blob Storage.
  - `vendorLogoUrl` is updated in Additional Info.
  - Any previous logo blob may be deleted.
- Response `data` is the full updated `AdditionalInfoData`.

Error contract is the same shape as vendor contract upload.

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
```

### Additional Info API helpers

```ts
// GET /additional-info
export const getAdditionalInfo = (
  orgId: number,
  vendorId: number
): Promise<AdditionalInfoData | null> =>
  api<AdditionalInfoData | null>(`/${orgId}/vendors/${vendorId}/additional-info`);

// PUT /additional-info
export const saveAdditionalInfo = (
  orgId: number,
  vendorId: number,
  payload: AdditionalInfoSaveRequest
): Promise<AdditionalInfoData> =>
  api<AdditionalInfoData>(`/${orgId}/vendors/${vendorId}/additional-info`, {
    method: "PUT",
    body: payload,
  });

// POST /additional-info/vendor-contract/upload
export const uploadVendorContract = async (
  orgId: number,
  vendorId: number,
  file: File
): Promise<AdditionalInfoData> => {
  const formData = new FormData();
  formData.append("file", file);

  const res = await fetch(
    `${API_BASE}/${orgId}/vendors/${vendorId}/additional-info/vendor-contract/upload`,
    {
      method: "POST",
      headers: {
        Authorization: `Bearer ${getToken()}`,
      },
      body: formData,
    }
  );

  const json: {
    success: boolean;
    message: string;
    data: AdditionalInfoData;
  } = await res.json();

  if (!res.ok || !json.success) {
    throw new Error(json.message || "Upload failed");
  }
  return json.data;
};

// POST /additional-info/vendor-logo/upload
export const uploadVendorLogo = async (
  orgId: number,
  vendorId: number,
  file: File
): Promise<AdditionalInfoData> => {
  const formData = new FormData();
  formData.append("file", file);

  const res = await fetch(
    `${API_BASE}/${orgId}/vendors/${vendorId}/additional-info/vendor-logo/upload`,
    {
      method: "POST",
      headers: {
        Authorization: `Bearer ${getToken()}`,
      },
      body: formData,
    }
  );

  const json: {
    success: boolean;
    message: string;
    data: AdditionalInfoData;
  } = await res.json();

  if (!res.ok || !json.success) {
    throw new Error(json.message || "Upload failed");
  }
  return json.data;
};
```

---

## UI Integration Flow (Additional Info section)

### 1. Visibility

Show the Additional Info section when the organization category is one of:

- `PHOTOGRAPHY`
- `PLANNING`
- `DECOR`
- `PLANNING_AND_DECOR`

### 2. Initial load

1. Use `getOrganizationWithDetails(orgId)` to get `organization` + `vendors`.
2. Determine `vendorId` (current pattern: `vendors[0].id`).
3. Call `getAdditionalInfo(orgId, vendorId)`:
   - If `data === null`, initialize your local form state with empty strings/`null` and an empty `customFields` array.
   - Otherwise, populate the form from the returned `AdditionalInfoData`.

### 3. Edit flow

- When the user clicks **Edit**:
  - Unlock the Additional Info form fields:
    - Starting price for 2 day wedding
    - Wedding take per day
    - Turnaround time for edited photos / videos
    - Specialize in photography style
    - Charges for travel and accommodation separate (dropdown: yes/no)
    - Custom fields (add/remove, edit label + value).
  - For custom fields, maintain an internal `id` for React keying, but send only `label`, `type`, and `value` to the backend.

### 4. Save

- On **Save**:
  - Build an `AdditionalInfoSaveRequest` from form state.
  - Map UI dropdown to `"yes" | "no" | ""` as needed.
  - Build `customFields` array with `{ label, type, value }`.
  - Call `saveAdditionalInfo(orgId, vendorId, body)`.
  - Replace your local state with the returned `AdditionalInfoData` on success.

### 5. Cancel

- On **Cancel**:
  - Discard unsaved changes.
  - Either:
    - Revert to the snapshot taken when entering edit mode, or
    - Re-fetch via `getAdditionalInfo`.

### 6. File uploads

- For **Vendor contract**:
  - On file select / Upload click, call `uploadVendorContract`.
  - On success, update `vendorContractUrl` from `data.vendorContractUrl`.
- For **Vendor logo**:
  - On file select / Upload click, call `uploadVendorLogo`.
  - On success, update `vendorLogoUrl` from `data.vendorLogoUrl`.
- Show backend error messages in a toast/snackbar when uploads fail.

---

## Field Mapping (UI ↔ API)

| UI Label                                       | Form Key                             | API Key                      |
|-----------------------------------------------|--------------------------------------|------------------------------|
| Starting price for 2 day wedding              | `additionalStartingPriceTwoDayWedding` | `startingPriceTwoDayWedding` |
| Wedding take per day                          | `additionalWeddingPerDay`            | `weddingPerDay`              |
| Turnaround time for edited photos / videos    | `additionalTurnaroundTime`           | `turnaroundTime`             |
| Specialize in photography style               | `additionalPhotographyStyle`         | `photographyStyle`           |
| Charges for travel and accommodation separate | `additionalTravelAccommodationCharges` | `travelAccommodationSeparate` |
| Vendor contract                               | (file upload field)                  | `vendorContractUrl`          |
| Vendor logo                                   | `additionalVendorLogoUrl` + upload   | `vendorLogoUrl`              |
| Custom fields                                 | local array of dynamic rows          | `customFields[]`             |

---

