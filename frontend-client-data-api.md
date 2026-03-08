# Client Data API – Frontend Integration Guide

This document describes the APIs for managing **Client Data** (Quote Format and Client Contract Format PDFs) on the Organization Details page (CLIENT DATA section).

---

## Overview

Client data stores two PDF URLs per organization:
- **Quote Format** – PDF for quote format (uploaded to Azure Blob Storage)
- **Client Contract Format** – PDF for client contract format (uploaded to Azure Blob Storage)

PDFs are uploaded to Azure Blob Storage; the table stores the public URLs. One record per organization.

---

## Base URL & Auth

**Base URL:** `/api/organizations`

**Auth:** `Authorization: Bearer <token>` on all endpoints

---

## Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/{orgId}/client-data` | Get client data (returns null if none) |
| POST | `/{orgId}/client-data/quote-format/upload` | Upload quote format PDF (multipart) |
| POST | `/{orgId}/client-data/client-contract-format/upload` | Upload client contract format PDF (multipart) |
| DELETE | `/{orgId}/client-data/quote-format` | Remove quote format PDF |
| DELETE | `/{orgId}/client-data/client-contract-format` | Remove client contract format PDF |
| DELETE | `/{orgId}/client-data` | Delete all client data |

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
GET returns `data: null` when no client data exists.

---

## TypeScript Types

```ts
interface ClientData {
  id: number;
  organizationId: number;
  quoteFormatUrl: string | null;
  clientContractFormatUrl: string | null;
  createdAt: string;
  updatedAt: string;
}
```

---

## API Details

### 1. Get Client Data

**GET** `/{orgId}/client-data`

**Response:** `data` is `ClientData` or `null` if no record exists.

**Example response (existing):**
```json
{
  "success": true,
  "message": "Client data fetched",
  "data": {
    "id": 1,
    "organizationId": 5,
    "quoteFormatUrl": "https://bridesideimages.blob.core.windows.net/call-screenshots/client-data/1234567890-abc12345-quote.pdf",
    "clientContractFormatUrl": "https://bridesideimages.blob.core.windows.net/call-screenshots/client-data/1234567891-def67890-contract.pdf",
    "createdAt": "2026-03-07T10:00:00",
    "updatedAt": "2026-03-07T10:00:00"
  }
}
```

**Example response (none):**
```json
{
  "success": true,
  "message": "Client data fetched",
  "data": null
}
```

---

### 2. Upload Quote Format PDF

**POST** `/{orgId}/client-data/quote-format/upload`

**Content-Type:** `multipart/form-data`

**Request:** Form field `file` with PDF file.

| Constraint | Value |
|------------|-------|
| File type | PDF only (`application/pdf`) |
| Max size | 10MB |

**Response:** `data` is the updated `ClientData` (HTTP 201). Replaces existing PDF if any.

---

### 3. Upload Client Contract Format PDF

**POST** `/{orgId}/client-data/client-contract-format/upload`

**Content-Type:** `multipart/form-data`

**Request:** Form field `file` with PDF file. Same constraints as quote format.

**Response:** `data` is the updated `ClientData` (HTTP 201).

---

### 4. Remove Quote Format

**DELETE** `/{orgId}/client-data/quote-format`

Removes the quote format PDF URL and deletes the file from Azure Blob Storage.

**Response:** `data` is the updated `ClientData` with `quoteFormatUrl: null`.

---

### 5. Remove Client Contract Format

**DELETE** `/{orgId}/client-data/client-contract-format`

Removes the client contract format PDF URL and deletes the file from Azure Blob Storage.

**Response:** `data` is the updated `ClientData` with `clientContractFormatUrl: null`.

---

### 6. Delete All Client Data

**DELETE** `/{orgId}/client-data`

Removes both PDFs and the client data record.

**Response:** `data` is null. HTTP 200 on success.

---

## API Client (TypeScript)

```ts
const API_BASE = "/api/organizations";

async function api<T>(
  path: string,
  options: { method?: string; body?: BodyInit; headers?: HeadersInit } = {}
): Promise<T> {
  const { method = "GET", body, headers = {} } = options;
  const res = await fetch(`${API_BASE}${path}`, {
    method,
    headers: {
      Authorization: `Bearer ${getToken()}`,
      ...(body instanceof FormData ? {} : { "Content-Type": "application/json" }),
      ...headers,
    },
    ...(body && { body }),
  });

  const json = await res.json();
  if (!res.ok || !json.success) {
    throw new Error(json.message || "Request failed");
  }
  return json.data;
}

// Client data
export const getClientData = (orgId: number) =>
  api<ClientData | null>(`/${orgId}/client-data`);

export const uploadQuoteFormat = (orgId: number, file: File) => {
  const formData = new FormData();
  formData.append("file", file);
  return api<ClientData>(`/${orgId}/client-data/quote-format/upload`, {
    method: "POST",
    body: formData,
  });
};

export const uploadClientContractFormat = (orgId: number, file: File) => {
  const formData = new FormData();
  formData.append("file", file);
  return api<ClientData>(`/${orgId}/client-data/client-contract-format/upload`, {
    method: "POST",
    body: formData,
  });
};

export const removeQuoteFormat = (orgId: number) =>
  api<ClientData>(`/${orgId}/client-data/quote-format`, { method: "DELETE" });

export const removeClientContractFormat = (orgId: number) =>
  api<ClientData>(`/${orgId}/client-data/client-contract-format`, {
    method: "DELETE",
  });

export const deleteClientData = (orgId: number) =>
  api<void>(`/${orgId}/client-data`, { method: "DELETE" });
```

---

## UI Integration Flow

### CLIENT DATA section

1. **Initial load:** Call `GET /{orgId}/client-data`. If `data` is null, show empty state (no PDFs).
2. **Upload Quote Format:** "Upload PDF" button → file picker (PDF only) → `POST /{orgId}/client-data/quote-format/upload` with `FormData`.
3. **Upload Client Contract Format:** Same flow with `POST /{orgId}/client-data/client-contract-format/upload`.
4. **View PDF:** Use `quoteFormatUrl` or `clientContractFormatUrl` as link `href` or in iframe.
5. **Remove PDF:** Add a remove/delete control → `DELETE /{orgId}/client-data/quote-format` or `.../client-contract-format`.

### Field mapping

| UI Field | API field |
|----------|-----------|
| Quote Format | `quoteFormatUrl` |
| Client Contract Format | `clientContractFormatUrl` |

### File validation (frontend)

- Accept only `.pdf` files
- Max size: 10MB
- Show error if user selects non-PDF

---

## Error Handling

| HTTP | Meaning |
|------|---------|
| 404 | Organization or client data not found |
| 401 | Unauthorized – invalid or missing token |
| 403 | Forbidden – insufficient permissions |
| 400 | Bad request – not a PDF, file too large, or validation error |
| 503 | Azure Blob Storage not configured |

Always check `res.ok` and `body.success` before using `body.data`.

---

## Azure Configuration

Ensure these environment variables are set:

- `AZURE_STORAGE_BLOB_CONNECTION_STRING` – Azure Storage connection string
- `AZURE_STORAGE_BLOB_CONTAINER_NAME` – Container name (default: `call-screenshots`)

PDFs are stored under the `client-data/` path prefix in the same container.

---

## Related Docs

- `frontend-api-integration-guide.md` – Full Organization Details page integration
- `AZURE_BLOB_STORAGE_SETUP.md` – Azure Blob Storage setup
