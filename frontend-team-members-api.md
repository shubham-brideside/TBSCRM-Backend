# Vendor Team Members API – Frontend Integration Guide

This document describes the APIs for managing **Vendor Team Members** on the Organization Details page (TEAM MEMBERS section).

---

## Overview

Team members are stored per vendor and include:
- **Name** – Team member name (required)
- **Designation** – Role/title (e.g. "Senior artist", "Junior artist")
- **Instagram ID** – Instagram handle (e.g. "@handle")

---

## Base URL & Auth

**Base URL:** `/api/organizations`

**Auth:** `Authorization: Bearer <token>` on all endpoints

---

## Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/{orgId}/vendors/{vendorId}/team-members` | List all team members for a vendor |
| POST | `/{orgId}/vendors/{vendorId}/team-members` | Create a new team member |
| PUT | `/{orgId}/vendors/{vendorId}/team-members/{memberId}` | Update a team member |
| DELETE | `/{orgId}/vendors/{vendorId}/team-members/{memberId}` | Delete a team member |

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
interface TeamMember {
  id: number;
  vendorId: number;
  name: string;
  designation: string | null;
  instagramId: string | null;
  createdAt: string;
  updatedAt: string;
}

interface TeamMemberCreateRequest {
  name: string;
  designation?: string | null;
  instagramId?: string | null;
}

interface TeamMemberUpdateRequest {
  name: string;
  designation?: string | null;
  instagramId?: string | null;
}
```

---

## API Details

### 1. List Team Members

**GET** `/{orgId}/vendors/{vendorId}/team-members`

**Response:** `data` is an array of `TeamMember`.

**Example response:**
```json
{
  "success": true,
  "message": "Vendor team members fetched",
  "data": [
    {
      "id": 1,
      "vendorId": 10,
      "name": "Riya",
      "designation": "Senior artist",
      "instagramId": "@riya_art",
      "createdAt": "2026-03-07T10:00:00",
      "updatedAt": "2026-03-07T10:00:00"
    },
    {
      "id": 2,
      "vendorId": 10,
      "name": "Shikha",
      "designation": "Junior artist",
      "instagramId": "@shikha_makeup",
      "createdAt": "2026-03-07T10:05:00",
      "updatedAt": "2026-03-07T10:05:00"
    }
  ]
}
```

---

### 2. Create Team Member

**POST** `/{orgId}/vendors/{vendorId}/team-members`

**Request body:**
```json
{
  "name": "Riya",
  "designation": "Senior artist",
  "instagramId": "@riya_art"
}
```

| Field | Type | Required | Notes |
|-------|------|----------|-------|
| name | string | Yes | Max 255 chars |
| designation | string | No | Max 255 chars |
| instagramId | string | No | Max 255 chars, e.g. "@handle" |

**Response:** `data` is the created `TeamMember` (HTTP 201).

---

### 3. Update Team Member

**PUT** `/{orgId}/vendors/{vendorId}/team-members/{memberId}`

**Request body:**
```json
{
  "name": "Riya Sharma",
  "designation": "Lead artist",
  "instagramId": "@riya_sharma_art"
}
```

Same fields as create. All fields are required in the update request (full replacement).

**Response:** `data` is the updated `TeamMember`.

---

### 4. Delete Team Member

**DELETE** `/{orgId}/vendors/{vendorId}/team-members/{memberId}`

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

// Team members
export const listTeamMembers = (orgId: number, vendorId: number) =>
  api<TeamMember[]>(`/${orgId}/vendors/${vendorId}/team-members`);

export const createTeamMember = (
  orgId: number,
  vendorId: number,
  body: TeamMemberCreateRequest
) =>
  api<TeamMember>(`/${orgId}/vendors/${vendorId}/team-members`, {
    method: "POST",
    body,
  });

export const updateTeamMember = (
  orgId: number,
  vendorId: number,
  memberId: number,
  body: TeamMemberUpdateRequest
) =>
  api<TeamMember>(`/${orgId}/vendors/${vendorId}/team-members/${memberId}`, {
    method: "PUT",
    body,
  });

export const deleteTeamMember = (
  orgId: number,
  vendorId: number,
  memberId: number
) =>
  api<void>(`/${orgId}/vendors/${vendorId}/team-members/${memberId}`, {
    method: "DELETE",
  });
```

---

## UI Integration Flow

### TEAM MEMBERS section

1. **Initial load:** Call `GET /{orgId}/vendors/{vendorId}/team-members` to populate the table.
2. **Add member:** "+ Team member" button opens a form. On submit, call `POST /{orgId}/vendors/{vendorId}/team-members`.
3. **Edit member:** "..." menu → Edit. On save, call `PUT /{orgId}/vendors/{vendorId}/team-members/{memberId}`.
4. **Delete member:** "..." menu → Delete. On confirm, call `DELETE /{orgId}/vendors/{vendorId}/team-members/{memberId}`.
5. **Save/Cancel:** If using a batch-edit pattern (edit locally, then Save), call individual PUTs for each changed row and DELETE for each removed row. Alternatively, call APIs immediately on each action.

### Field mapping

| UI Column | API field |
|-----------|-----------|
| Team member | `name` |
| Designation | `designation` |
| Instagram ID | `instagramId` |

---

## Error Handling

| HTTP | Meaning |
|------|---------|
| 404 | Organization, vendor, or team member not found |
| 401 | Unauthorized – invalid or missing token |
| 403 | Forbidden – insufficient permissions |
| 400 | Bad request – validation error (e.g. missing name) |

Always check `res.ok` and `body.success` before using `body.data`.

---

## Related Docs

- `frontend-api-integration-guide.md` – Full Organization Details page integration
- `frontend-brideside-vendors-api.md` – Vendor API details
