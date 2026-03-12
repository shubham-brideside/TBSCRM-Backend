# Organization Onboarding Progress API – Frontend Integration Guide

This document describes the APIs for **Organization Onboarding Progress** and **`is_active`** status. Use this when integrating the Organization Details page, pipeline creation, and organization selection flows.

---

## Overview

- **`is_active`**: An organization becomes active only when all onboarding sections are complete:
  1. Organization details
  2. Asset info
  3. Events pricing
  4. Vendor data
  5. Client data
  6. Team members

- **Pipeline creation**: Pipelines can only be created for organizations where `is_active` is `true`.

- **Progress tracking**: The backend provides a progress endpoint and updates progress automatically when any section changes.

---

## Base URL & Auth

**Base URL:** `/api/organizations`

**Auth:** `Authorization: Bearer <token>` on all endpoints

---

## Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/{orgId}/progress` | Get onboarding progress (completion status per section) |
| GET | `/{orgId}/progress/debug` | Same as progress + `clientDataRecordExists`, `quoteFormatUrlPresent`, `clientContractFormatUrlPresent` (for diagnosing `clientDataComplete`) |
| GET | `/{id}` | Get organization (includes `isActive`) |
| GET | `accessible-for-current-user` | List accessible organizations (includes `isActive`) |

---

## Response Envelope

All responses use:

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
// Organization (now includes isActive)
interface Organization {
  id: number;
  name: string;
  category: string;
  address: string | null;
  email: string | null;
  owner: { id: number; firstName: string; lastName: string; email: string } | null;
  isActive: boolean;  // NEW: true when all onboarding sections complete
  createdAt: string;
  updatedAt: string;
}

// Organization onboarding progress
interface OrganizationProgress {
  organizationId: number;
  organizationDetailsComplete: boolean;
  assetInfoComplete: boolean;
  eventsPricingComplete: boolean;
  vendorDataComplete: boolean;
  clientDataComplete: boolean;
  teamMembersComplete: boolean;
  isActive: boolean;
  completedCount: number;  // 0–6
  totalCount: number;     // 6
  updatedAt: string;
}
```

---

## API Details

### 1. Get Organization Progress

**GET** `/{orgId}/progress`

**Response:** `data` is `OrganizationProgress`.

**Example response:**
```json
{
  "success": true,
  "message": "Organization progress fetched",
  "data": {
    "organizationId": 5,
    "organizationDetailsComplete": true,
    "assetInfoComplete": true,
    "eventsPricingComplete": false,
    "vendorDataComplete": true,
    "clientDataComplete": false,
    "teamMembersComplete": true,
    "isActive": false,
    "completedCount": 4,
    "totalCount": 6,
    "updatedAt": "2026-03-11T10:30:00"
  }
}
```

---

### 2. Get Organization (includes `isActive`)

**GET** `/{id}`

**Response:** `data` includes `isActive: boolean`.

---

### 3. List Accessible Organizations (includes `isActive`)

**GET** `/accessible-for-current-user`

**Response:** Array of organizations, each with `isActive`.

---

## UI Integration Flow

### Organization Details Page

1. **On load:** Call `GET /{orgId}/progress` to show the progress bar/section.
2. **Progress bar:** Use `completedCount` and `totalCount` (e.g. `4/6`).
3. **Section checklist:** Map each `*Complete` field to the corresponding section:
   - `organizationDetailsComplete` → Organization info
   - `assetInfoComplete` → Asset info
   - `eventsPricingComplete` → Events pricing
   - `vendorDataComplete` → Vendor data
   - `clientDataComplete` → Client data
   - `teamMembersComplete` → Team members
4. **After save:** Progress is automatically updated when any section is saved. No need to call `GET /progress` again unless you want to refresh.

### Pipeline Creation

1. **Organization dropdown:** Only show organizations where `isActive === true`.
2. **Filter:** Use `GET /accessible-for-current-user` and filter `data.filter(o => o.isActive)`.
3. **Error handling:** If the user selects an inactive org (e.g. from cache), you may receive `400` with `"Organization must be active (all onboarding details complete) to create a pipeline..."`.

### Organization List / Dropdown

- **Active badge:** Show `isActive` as a badge (e.g. “Active” vs “Setup incomplete”).
- **Filter:** Use `isActive` to filter or sort organizations.

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

// Progress
export const getOrganizationProgress = (orgId: number) =>
  api<OrganizationProgress>(`/${orgId}/progress`);

// Organizations (filtered for pipeline creation)
export const getActiveOrganizations = async () => {
  const orgs = await api<Organization[]>(`/accessible-for-current-user`);
  return orgs.filter((o) => o.isActive);
};
```

---

## Progress Bar Component Example

```tsx
function OrganizationProgressBar({ orgId }: { orgId: number }) {
  const { data: progress, isLoading } = useQuery(
    ["organization-progress", orgId],
    () => getOrganizationProgress(orgId)
  );

  if (isLoading || !progress) return null;

  const sections = [
    { key: "organizationDetailsComplete", label: "Organization details" },
    { key: "assetInfoComplete", label: "Asset info" },
    { key: "eventsPricingComplete", label: "Events pricing" },
    { key: "vendorDataComplete", label: "Vendor data" },
    { key: "clientDataComplete", label: "Client data" },
    { key: "teamMembersComplete", label: "Team members" },
  ];

  return (
    <div>
      <div className="progress-bar">
        <div
          style={{ width: `${(progress.completedCount / progress.totalCount) * 100}%` }}
        />
      </div>
      <p>{progress.completedCount} of {progress.totalCount} sections complete</p>
      {progress.isActive && <span className="badge">Ready for pipeline</span>}
      <ul>
        {sections.map(({ key, label }) => (
          <li key={key}>
            {progress[key as keyof OrganizationProgress] ? "✓" : "○"} {label}
          </li>
        ))}
      </ul>
    </div>
  );
}
```

---

## Section Completion Rules

| Section | Required | Notes |
|---------|----------|-------|
| Organization details | vendorName, contactNumber, emailId, (accountOwner or organization.owner), and at least one of officeStudioLocation or baseLocation | Stored in `brideside_vendors` table, per vendor |
| Asset info | At least one vendor asset per vendor with meaningful data (phone_model, sim_card, issued_on, phone_issued_by, or sim_issued_by) | Vendor assets table |
| Events pricing | At least one event pricing row per vendor | Events pricing table |
| Vendor data | At least one of master data link or calendar sheet link per vendor | Vendor data table |
| Client data | Both quote format and client contract format PDFs | Client data table |
| Team members | At least one team member per vendor | Vendor team members table |

---

## Error Handling

| HTTP | Meaning |
|------|---------|
| 404 | Organization not found |
| 401 | Unauthorized – invalid or missing token |
| 403 | Forbidden – insufficient permissions |
| 400 | Bad request – e.g. pipeline creation with inactive organization |

**Pipeline creation error example:**
```json
{
  "success": false,
  "message": "Organization must be active (all onboarding details complete) to create a pipeline. Complete Organization details, Asset Info, Events Pricing, Vendor Data, Client Data, and Team Members."
}
```

---

## Related Docs

- `frontend-api-integration-guide.md` – Full Organization Details page integration
- `frontend-pipelines-api.md` – Pipeline creation and endpoints
- `frontend-client-data-api.md` – Client data (quote/contract PDFs)
- `frontend-vendor-data-api.md` – Vendor data (URLs)
- `frontend-team-members-api.md` – Team members
