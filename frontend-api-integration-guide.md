# Frontend API Integration Guide – Organization Details Page

This guide consolidates all APIs needed to integrate the **Organization Details** page with the backend. Use it as the primary reference for frontend development.

---

## Quick Reference

| Section | GET | POST | PUT | PATCH | DELETE |
|---------|-----|------|-----|-------|--------|
| **Organization** | `/{id}`, `/{id}/with-details`, `/{id}/progress` | `/` | `/{id}` | — | — |
| **Vendors** | `/{id}/vendors` | `/{id}/vendors` | `/{id}/vendors/{vendorId}` | `/{id}/vendors/{vendorId}/about` | — |
| **Event Pricing** | (in vendor response) | — | `/{id}/vendors/{vendorId}/event-pricing` | — | — |
| **Vendor Assets** | `/{id}/vendors/{vendorId}/assets` | — | `/{id}/vendors/{vendorId}/assets/{assetId}` | — | — |
| **Team Members** | `/{id}/vendors/{vendorId}/team-members` | `/{id}/vendors/{vendorId}/team-members` | `/{id}/vendors/{vendorId}/team-members/{memberId}` | — | `/{id}/vendors/{vendorId}/team-members/{memberId}` |
| **Vendor Data** | `/{id}/vendors/{vendorId}/vendor-data` | `/{id}/vendors/{vendorId}/vendor-data` | `/{id}/vendors/{vendorId}/vendor-data` or `.../vendor-data/{dataId}` | — | `/{id}/vendors/{vendorId}/vendor-data` |
| **Team Size (table)** | `/{id}/vendors/{vendorId}/team-size` | — | `/{id}/vendors/{vendorId}/team-size` | — | — |
| **Client Data** | `/{id}/client-data` | `/{id}/client-data/quote-format/upload`, `.../client-contract-format/upload` | — | — | `/{id}/client-data/quote-format`, `.../client-contract-format`, `/{id}/client-data` |

**Base URL:** `/api/organizations`

**Auth:** `Authorization: Bearer <token>` on all endpoints

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
// Shared
interface ApiResponse<T> {
  success: boolean;
  message: string;
  data: T;
}

// Organization
interface Organization {
  id: number;
  name: string;
  category: string;
  address: string | null;
  email: string | null;
  owner: { id: number; firstName: string; lastName: string; email: string } | null;
  isActive: boolean;  // true when all onboarding sections complete
  createdAt: string;
  updatedAt: string;
}

// Account owner (nested in vendor)
interface AccountOwnerSummary {
  id: number;
  firstName: string;
  lastName: string;
  email: string;
}

// Event pricing (nested in vendor)
interface ArtistPricing {
  basePrice: number | null;
  destinationPrice: number | null;
  additionalMakeupPrice: number | null;
  availabilityAtStudio: string | null;
  policyNotes: string | null;
  currency: string | null;
}

interface EventPricingRow {
  code: string;
  label: string;
  primary: ArtistPricing | null;
  senior: ArtistPricing | null;
  junior: ArtistPricing | null;
}

// Vendor (full response)
interface Vendor {
  id: number;
  username: string;
  organizationId: number;
  pipelineId: number;
  igAccountId: string | null;
  businessName: string | null;
  vendorName: string | null;
  about: string | null;
  services: string | null;
  eventPricing: EventPricingRow[];
  eventPricingCurrent: EventPricingRow[];
  eventPricingUpcoming: EventPricingRow[];
  accountOwner: AccountOwnerSummary | null;
  contactNumber: string | null;
  officeStudioLocation: string | null;
  baseLocation: string | null;
  officialNumber: string | null;
  emailId: string | null;
  onboardingDate: string | null;
  teamSize: number | null;
  onboardingFee: number | null;
  createdAt: string;
  updatedAt: string;
}

// Organization with details (single call)
interface OrganizationWithDetails {
  organization: Organization;
  vendors: Vendor[];
}

// Vendor data (URLs)
interface VendorData {
  id: number;
  vendorId: number;
  masterDataLink: string | null;
  calendarSheetLink: string | null;
  createdAt: string;
  updatedAt: string;
}

// Vendor asset
interface VendorAsset {
  id: number;
  vendorId: number;
  organizationId: number;
  phoneModel: string | null;
  phoneIssuedBy: string | null;
  simCard: string | null;
  simIssuedBy: string | null;
  issuedOn: string | null;
  createdAt: string;
  updatedAt: string;
}
```

---

## API Client

```ts
const API_BASE = "/api/organizations"; // or your proxy base

async function api<T>(
  path: string,
  options: { method?: string; body?: object } = {}
): Promise<T> {
  const { method = "GET", body } = options;
  const res = await fetch(`${API_BASE}${path}`, {
    method,
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${getToken()}`, // your auth helper
    },
    ...(body && { body: JSON.stringify(body) }),
  });

  const json: ApiResponse<T> = await res.json();
  if (!res.ok || !json.success) {
    throw new Error(json.message || "Request failed");
  }
  return json.data;
}

// Organization
export const getOrganization = (id: number) =>
  api<Organization>(`/${id}`);

export const getOrganizationWithDetails = (id: number) =>
  api<OrganizationWithDetails>(`/${id}/with-details`);

// Vendors
export const listVendors = (orgId: number) =>
  api<Vendor[]>(`/${orgId}/vendors`);

export const createVendor = (orgId: number, body?: object) =>
  api<Vendor>(`/${orgId}/vendors`, { method: "POST", body: body ?? {} });

export const updateVendor = (orgId: number, vendorId: number, body: object) =>
  api<Vendor>(`/${orgId}/vendors/${vendorId}`, { method: "PUT", body });

export const updateVendorAbout = (orgId: number, vendorId: number, about: string | null) =>
  api<Vendor>(`/${orgId}/vendors/${vendorId}/about`, {
    method: "PATCH",
    body: { about },
  });

// Event pricing
export const saveEventPricing = (
  orgId: number,
  vendorId: number,
  body: {
    eventPricing?: EventPricingRow[] | null;
    eventPricingCurrent?: EventPricingRow[] | null;
    eventPricingUpcoming?: EventPricingRow[] | null;
  }
) =>
  api<Vendor>(`/${orgId}/vendors/${vendorId}/event-pricing`, {
    method: "PUT",
    body,
  });

// Vendor assets
export const listVendorAssets = (orgId: number, vendorId: number) =>
  api<VendorAsset[]>(`/${orgId}/vendors/${vendorId}/assets`);

export const updateVendorAsset = (
  orgId: number,
  vendorId: number,
  assetId: number,
  body: Partial<VendorAsset>
) =>
  api<VendorAsset>(`/${orgId}/vendors/${vendorId}/assets/${assetId}`, {
    method: "PUT",
    body,
  });

// Team members
export const listTeamMembers = (orgId: number, vendorId: number) =>
  api<VendorTeamMember[]>(`/${orgId}/vendors/${vendorId}/team-members`);

export const createTeamMember = (
  orgId: number,
  vendorId: number,
  body: { name: string; designation?: string | null; instagramId?: string | null }
) =>
  api<VendorTeamMember>(`/${orgId}/vendors/${vendorId}/team-members`, {
    method: "POST",
    body,
  });

export const updateTeamMember = (
  orgId: number,
  vendorId: number,
  memberId: number,
  body: { name: string; designation?: string | null; instagramId?: string | null }
) =>
  api<VendorTeamMember>(`/${orgId}/vendors/${vendorId}/team-members/${memberId}`, {
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

// Vendor data (URLs)
export const getVendorData = (orgId: number, vendorId: number) =>
  api<VendorData | null>(`/${orgId}/vendors/${vendorId}/vendor-data`);

export const saveVendorData = (
  orgId: number,
  vendorId: number,
  body: { masterDataLink?: string | null; calendarSheetLink?: string | null }
) =>
  api<VendorData>(`/${orgId}/vendors/${vendorId}/vendor-data`, {
    method: "PUT",
    body,
  });

export const deleteVendorData = (orgId: number, vendorId: number) =>
  api<void>(`/${orgId}/vendors/${vendorId}/vendor-data`, {
    method: "DELETE",
  });

// Team size (detailed table)
export interface TeamSizeRow {
  id?: number;
  guestCount: string;
  eventType: string;
  photographer?: string;
  cinematographer?: string;
  drone?: string;
  notes?: string;
}

export const getTeamSizeRows = (orgId: number, vendorId: number) =>
  api<{ rows: TeamSizeRow[] }>(`/${orgId}/vendors/${vendorId}/team-size`);

export const saveTeamSizeRows = (
  orgId: number,
  vendorId: number,
  rows: TeamSizeRow[]
) =>
  api<{ rows: TeamSizeRow[] }>(`/${orgId}/vendors/${vendorId}/team-size`, {
    method: "PUT",
    body: { rows },
  });
```

---

## Page Flow: Organization Details

### 1. Initial load

```ts
// Single call – organization + vendors (with event pricing, about, etc.)
const { organization, vendors } = await getOrganizationWithDetails(orgId);
```

### 2. Section-specific flows

| Section | Data source | Save action |
|---------|-------------|-------------|
| **Organization info** | `organization` | `PUT /{orgId}` |
| **Vendor details** | `vendors[0]` | `PUT /{orgId}/vendors/{vendorId}` |
| **About** | `vendors[0].about` | `PATCH /{orgId}/vendors/{vendorId}/about` |
| **Events Pricing** | `vendors[0].eventPricing*` | `PUT /{orgId}/vendors/{vendorId}/event-pricing` |
| **Asset info** | `GET .../assets` | `PUT /{orgId}/vendors/{vendorId}/assets/{assetId}` |
| **Team Members** | `GET .../team-members` | `POST` create, `PUT` edit, `DELETE` remove |
| **Vendor Data** | `GET .../vendor-data` | `PUT /vendor-data` save (upsert), `DELETE` clear |
| **Team Size (table)** | `GET .../team-size` | `PUT .../team-size` (replace all rows) |
| **Client Data** | `GET .../client-data` | `POST .../quote-format/upload`, `.../client-contract-format/upload` (multipart), `DELETE` to remove |

### 3. When no vendor exists

If `vendors` is empty when loading, create one first:

```ts
if (vendors.length === 0) {
  await createVendor(orgId, {});
  const { vendors } = await getOrganizationWithDetails(orgId);
  // use vendors[0]
}
```

---

## Section-by-Section Integration

### Vendor details (contact, location, onboarding)

- **Read:** `vendors[0]` from `getOrganizationWithDetails`
- **Update:** `PUT /{orgId}/vendors/{vendorId}` with full payload
- **Fields:** `vendorName`, `contactNumber`, `officeStudioLocation`, `baseLocation`, `officialNumber`, `emailId`, `onboardingDate`, `teamSize`, `onboardingFee`

### About (vendor services description)

- **Read:** `vendors[0].about`
- **Update:** `PATCH /{orgId}/vendors/{vendorId}/about` with `{ "about": "..." }`
- **Note:** Can also be updated via full vendor `PUT`

### Events Pricing

Layout depends on `organization.category`:

| Category | Use | Payload |
|----------|-----|---------|
| Photography, Planning, Decor | `eventPricing` | Resource rows, `primary` only |
| Makeup | `eventPricingCurrent`, `eventPricingUpcoming` | Event rows, `primary`/`senior`/`junior` |

**Read:** `vendors[0].eventPricing`, `eventPricingCurrent`, `eventPricingUpcoming`

**Save:** `PUT /{orgId}/vendors/{vendorId}/event-pricing` – replaces all pricing. Send full arrays.

**Photography example:**

```json
{
  "eventPricing": [
    {
      "code": "Candid Photographer",
      "label": "Candid Photographer",
      "primary": {
        "basePrice": 50000,
        "policyNotes": "Full day coverage",
        "currency": "INR"
      },
      "senior": null,
      "junior": null
    }
  ],
  "eventPricingCurrent": null,
  "eventPricingUpcoming": null
}
```

**Makeup example:**

```json
{
  "eventPricing": null,
  "eventPricingCurrent": [
    {
      "code": "Wedding",
      "label": "Wedding",
      "primary": {
        "basePrice": 75000,
        "destinationPrice": 95000,
        "additionalMakeupPrice": 8000,
        "currency": "INR"
      },
      "senior": { "basePrice": 55000, "destinationPrice": 70000, "currency": "INR" },
      "junior": { "basePrice": 35000, "destinationPrice": 45000, "currency": "INR" }
    }
  ],
  "eventPricingUpcoming": []
}
```

### Asset info (phone, SIM)

- **Read:** `GET /{orgId}/vendors/{vendorId}/assets` → use `data[0]`
- **Update:** `PUT /{orgId}/vendors/{vendorId}/assets/{assetId}` with `phoneModel`, `phoneIssuedBy`, `simCard`, `simIssuedBy`, `issuedOn`

### Team Members

- **Read:** `GET /{orgId}/vendors/{vendorId}/team-members` → array of `{ id, vendorId, name, designation, instagramId }`
- **Create:** `POST /{orgId}/vendors/{vendorId}/team-members` with `{ name, designation?, instagramId? }`
- **Update:** `PUT /{orgId}/vendors/{vendorId}/team-members/{memberId}` with `{ name, designation?, instagramId? }`
- **Delete:** `DELETE /{orgId}/vendors/{vendorId}/team-members/{memberId}`

### Vendor Data (Master Data Link, Calendar Sheet Link)

- **Read:** `GET /{orgId}/vendors/{vendorId}/vendor-data` → `{ id, vendorId, masterDataLink, calendarSheetLink }` or `null`
- **Save:** `PUT /{orgId}/vendors/{vendorId}/vendor-data` with `{ masterDataLink?, calendarSheetLink? }` (upsert)
- **Delete:** `DELETE /{orgId}/vendors/{vendorId}/vendor-data`

### Client Data (Quote Format, Client Contract Format PDFs)

- **Read:** `GET /{orgId}/client-data` → `{ id, organizationId, quoteFormatUrl, clientContractFormatUrl }` or `null`
- **Upload Quote Format:** `POST /{orgId}/client-data/quote-format/upload` with `multipart/form-data`, field `file` (PDF, max 10MB)
- **Upload Client Contract Format:** `POST /{orgId}/client-data/client-contract-format/upload` (same)
- **Remove:** `DELETE /{orgId}/client-data/quote-format` or `.../client-contract-format`
- **Delete all:** `DELETE /{orgId}/client-data`

---

## Error Handling

| HTTP | Meaning |
|------|---------|
| 404 | Organization or vendor not found |
| 401 | Unauthorized – invalid or missing token |
| 403 | Forbidden – insufficient permissions |
| 400 | Bad request – validation error |

Always check `res.ok` and `body.success` before using `body.data`.

---

## Field Notes

- **`services`**: JSON string (e.g. `"[\"Photography\"]"`). Parse with `JSON.parse()` for arrays.
- **`onboardingFee`**: Decimal; format as currency in UI.
- **`onboardingDate`**, **`issuedOn`**: ISO-8601 format (e.g. `2026-03-02T00:00:00`). See `frontend-datetime-format-guide.md`.
- **`access_token`**: Not exposed by the API.

---

## Related Docs

- `frontend-datetime-format-guide.md` – Date/time format for `onboardingDate`, `issuedOn`, etc.
- `frontend-organization-onboarding-progress-api.md` – Organization progress, `isActive`, and pipeline creation rules
- `frontend-brideside-vendors-api.md` – Vendor API details
- `frontend-event-pricing-api.md` – Event pricing structure
- `frontend-vendor-about-api.md` – About field
- `frontend-vendor-assets-api.md` – Asset info
- `frontend-team-members-api.md` – Team members (name, designation, instagram)
- `frontend-vendor-data-api.md` – Vendor data (master data link, calendar sheet link)
- `frontend-client-data-api.md` – Client data (quote format, client contract format PDFs)
