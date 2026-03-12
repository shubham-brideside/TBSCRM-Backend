# Frontend API Integration Guide – Complete Reference

Single reference for integrating the CRM frontend with the backend. Covers Organizations, Onboarding Progress, Pipelines, Vendors, Client Data, Vendor Data, and Team Members.

---

## Base URL & Auth

| Base URL | Domain |
|----------|--------|
| `/api/organizations` | Organizations, vendors, progress, client data, vendor data, team members |
| `/api/pipelines` | Pipelines and stages |
| `/api/teams` | Teams (for pipeline dropdown) |

**Auth:** `Authorization: Bearer <token>` on all endpoints

---

## Response Envelope

```json
{
  "success": true,
  "message": "Human readable message",
  "data": { ... }
}
```

On error: `success: false`, `message` contains the error description.

---

# Part 1: Organizations & Onboarding Progress

## Quick Reference

| Section | GET | POST | PUT | PATCH | DELETE |
|---------|-----|------|-----|-------|--------|
| **Organization** | `/{id}`, `/{id}/with-details`, `/{id}/progress` | `/` | `/{id}` | — | — |
| **Vendors** | `/{id}/vendors` | `/{id}/vendors` | `/{id}/vendors/{vendorId}` | `/{id}/vendors/{vendorId}/about` | — |
| **Event Pricing** | (in vendor response) | — | `/{id}/vendors/{vendorId}/event-pricing` | — | — |
| **Vendor Assets** | `/{id}/vendors/{vendorId}/assets` | `/{id}/vendors/{vendorId}/assets` | `/{id}/vendors/{vendorId}/assets/{assetId}` | — | — |
| **Team Members** | `/{id}/vendors/{vendorId}/team-members` | `/{id}/vendors/{vendorId}/team-members` | `/{id}/vendors/{vendorId}/team-members/{memberId}` | — | `/{id}/vendors/{vendorId}/team-members/{memberId}` |
| **Vendor Data** | `/{id}/vendors/{vendorId}/vendor-data` | `/{id}/vendors/{vendorId}/vendor-data` | `/{id}/vendors/{vendorId}/vendor-data` | — | `/{id}/vendors/{vendorId}/vendor-data` |
| **Client Data** | `/{id}/client-data` | `/{id}/client-data/quote-format/upload`, `.../client-contract-format/upload` | — | — | `/{id}/client-data/quote-format`, `.../client-contract-format`, `/{id}/client-data` |

## Organization Onboarding Progress & is_active

- **`is_active`**: Organization becomes active only when all 6 sections are complete: Organization details, Asset info, Events pricing, Vendor data, Client data, Team members.
- **Pipeline creation**: Only organizations with `isActive=true` can be used for pipeline creation.
- **Progress**: Backend updates progress automatically when any section changes.

### GET /{orgId}/progress

Returns progress for each section:

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

### Organization Dropdown for Pipelines

- Use `GET /api/organizations/accessible-for-current-user` and filter `data.filter(o => o.isActive)`.
- Only show active organizations in the pipeline creation dropdown.

---

# Part 2: TypeScript Types

```ts
interface ApiResponse<T> {
  success: boolean;
  message: string;
  data: T;
}

interface Organization {
  id: number;
  name: string;
  category: string;
  address: string | null;
  email: string | null;
  owner: { id: number; firstName: string; lastName: string; email: string } | null;
  isActive: boolean;
  createdAt: string;
  updatedAt: string;
}

interface OrganizationProgress {
  organizationId: number;
  organizationDetailsComplete: boolean;
  assetInfoComplete: boolean;
  eventsPricingComplete: boolean;
  vendorDataComplete: boolean;
  clientDataComplete: boolean;
  teamMembersComplete: boolean;
  isActive: boolean;
  completedCount: number;
  totalCount: number;
  updatedAt: string;
}

interface OrganizationWithDetails {
  organization: Organization;
  vendors: Vendor[];
}

interface Vendor {
  id: number;
  username: string;
  organizationId: number;
  pipelineId: number;
  vendorName: string | null;
  about: string | null;
  eventPricing: EventPricingRow[];
  eventPricingCurrent: EventPricingRow[];
  eventPricingUpcoming: EventPricingRow[];
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

interface EventPricingRow {
  code: string;
  label: string;
  primary: ArtistPricing | null;
  senior: ArtistPricing | null;
  junior: ArtistPricing | null;
}

interface ArtistPricing {
  basePrice: number | null;
  destinationPrice: number | null;
  additionalMakeupPrice: number | null;
  currency: string | null;
  policyNotes: string | null;
}

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

interface VendorData {
  id: number;
  vendorId: number;
  masterDataLink: string | null;
  calendarSheetLink: string | null;
  createdAt: string;
  updatedAt: string;
}

interface ClientData {
  id: number;
  organizationId: number;
  quoteFormatUrl: string | null;
  clientContractFormatUrl: string | null;
  createdAt: string;
  updatedAt: string;
}

interface TeamMember {
  id: number;
  vendorId: number;
  name: string;
  designation: string | null;
  instagramId: string | null;
  createdAt: string;
  updatedAt: string;
}
```

---

# Part 3: API Client (TypeScript)

```ts
const ORG_API = "/api/organizations";
const PIPELINE_API = "/api/pipelines";

async function api<T>(base: string, path: string, options: { method?: string; body?: object } = {}): Promise<T> {
  const { method = "GET", body } = options;
  const res = await fetch(`${base}${path}`, {
    method,
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${getToken()}`,
    },
    ...(body && { body: JSON.stringify(body) }),
  });
  const json = await res.json();
  if (!res.ok || !json.success) throw new Error(json.message || "Request failed");
  return json.data;
}

// Organizations
export const getOrganization = (id: number) => api<Organization>(ORG_API, `/${id}`);
export const getOrganizationWithDetails = (id: number) => api<OrganizationWithDetails>(ORG_API, `/${id}/with-details`);
export const getOrganizationProgress = (id: number) => api<OrganizationProgress>(ORG_API, `/${id}/progress`);
export const getAccessibleOrganizations = () => api<Organization[]>(ORG_API, `/accessible-for-current-user`);
export const getActiveOrganizations = async () => {
  const orgs = await getAccessibleOrganizations();
  return orgs.filter((o) => o.isActive);
};
export const updateOrganization = (id: number, body: object) => api<Organization>(ORG_API, `/${id}`, { method: "PUT", body });

// Vendors
export const listVendors = (orgId: number) => api<Vendor[]>(ORG_API, `/${orgId}/vendors`);
export const createVendor = (orgId: number, body?: object) => api<Vendor>(ORG_API, `/${orgId}/vendors`, { method: "POST", body: body ?? {} });
export const updateVendor = (orgId: number, vendorId: number, body: object) => api<Vendor>(ORG_API, `/${orgId}/vendors/${vendorId}`, { method: "PUT", body });
export const saveEventPricing = (orgId: number, vendorId: number, body: object) => api<Vendor>(ORG_API, `/${orgId}/vendors/${vendorId}/event-pricing`, { method: "PUT", body });

// Vendor Assets
export const listVendorAssets = (orgId: number, vendorId: number) => api<VendorAsset[]>(ORG_API, `/${orgId}/vendors/${vendorId}/assets`);
export const createVendorAsset = (orgId: number, vendorId: number, body?: object) => api<VendorAsset>(ORG_API, `/${orgId}/vendors/${vendorId}/assets`, { method: "POST", body: body ?? {} });
export const updateVendorAsset = (orgId: number, vendorId: number, assetId: number, body: object) => api<VendorAsset>(ORG_API, `/${orgId}/vendors/${vendorId}/assets/${assetId}`, { method: "PUT", body });

// Team Members
export const listTeamMembers = (orgId: number, vendorId: number) => api<TeamMember[]>(ORG_API, `/${orgId}/vendors/${vendorId}/team-members`);
export const createTeamMember = (orgId: number, vendorId: number, body: { name: string; designation?: string | null; instagramId?: string | null }) => api<TeamMember>(ORG_API, `/${orgId}/vendors/${vendorId}/team-members`, { method: "POST", body });
export const updateTeamMember = (orgId: number, vendorId: number, memberId: number, body: object) => api<TeamMember>(ORG_API, `/${orgId}/vendors/${vendorId}/team-members/${memberId}`, { method: "PUT", body });
export const deleteTeamMember = (orgId: number, vendorId: number, memberId: number) => api<void>(ORG_API, `/${orgId}/vendors/${vendorId}/team-members/${memberId}`, { method: "DELETE" });

// Vendor Data
export const getVendorData = (orgId: number, vendorId: number) => api<VendorData | null>(ORG_API, `/${orgId}/vendors/${vendorId}/vendor-data`);
export const saveVendorData = (orgId: number, vendorId: number, body: { masterDataLink?: string | null; calendarSheetLink?: string | null }) => api<VendorData>(ORG_API, `/${orgId}/vendors/${vendorId}/vendor-data`, { method: "PUT", body });

// Client Data (PDFs – use FormData for upload)
export const getClientData = (orgId: number) => api<ClientData | null>(ORG_API, `/${orgId}/client-data`);
export const uploadQuoteFormat = (orgId: number, file: File) => {
  const formData = new FormData();
  formData.append("file", file);
  return fetch(`${ORG_API}/${orgId}/client-data/quote-format/upload`, {
    method: "POST",
    headers: { Authorization: `Bearer ${getToken()}` },
    body: formData,
  }).then(r => r.json()).then(j => j.success ? j.data : Promise.reject(new Error(j.message)));
};
export const uploadClientContractFormat = (orgId: number, file: File) => {
  const formData = new FormData();
  formData.append("file", file);
  return fetch(`${ORG_API}/${orgId}/client-data/client-contract-format/upload`, {
    method: "POST",
    headers: { Authorization: `Bearer ${getToken()}` },
    body: formData,
  }).then(r => r.json()).then(j => j.success ? j.data : Promise.reject(new Error(j.message)));
};
```

---

# Part 4: Pipelines

**Base URL:** `/api/pipelines`

## Create Pipeline

**POST** `/api/pipelines`

```json
{
  "name": "New Pipeline",
  "category": "Photography",
  "teamId": 42,
  "organizationId": 12
}
```

- `organizationId` must reference an active organization (`isActive=true`).
- Response (201): `PipelineResponse` with `stages` array.

## List / Get / Update / Delete Pipeline

**GET** `/api/pipelines?includeStages={boolean}` – List pipelines  
**GET** `/api/pipelines/{pipelineId}` – Get single  
**PATCH** `/api/pipelines/{pipelineId}` – Update  
**DELETE** `/api/pipelines/{pipelineId}?hard={boolean}` – Delete (soft by default)

## Stages

**GET** `/api/pipelines/{pipelineId}/stages` – List stages  
**POST** `/api/pipelines/{pipelineId}/stages` – Create stage  
**PATCH** `/api/pipelines/{pipelineId}/stages/{stageId}` – Update stage  
**DELETE** `/api/pipelines/{pipelineId}/stages/{stageId}` – Delete stage  
**POST** `/api/pipelines/{pipelineId}/stages/reorder` – Body: `{ "orderedStageIds": [12, 18, 5, 7] }`

## Dropdowns

- **Teams:** `GET /api/teams` → `{ id, name }`
- **Organizations:** `GET /api/organizations/accessible-for-current-user` → filter by `isActive`
- **Categories:** `GET /api/pipelines/categories` → `{ code, label }`

---

# Part 5: Page Flows

## Organization Details Page

1. **On load:** `getOrganizationWithDetails(orgId)` and `getOrganizationProgress(orgId)`.
2. **Progress bar:** Use `completedCount` / `totalCount`.
3. **Section checklist:** Map each `*Complete` field to the corresponding section.
4. **Save actions:** See Quick Reference table above.

## Pipeline Creation

1. **Organization dropdown:** Use `getActiveOrganizations()`.
2. **Teams dropdown:** `GET /api/teams`.
3. **On error 400:** Show message if organization is inactive.

## Client Data (PDFs)

- **Upload:** `POST /{orgId}/client-data/quote-format/upload` with `multipart/form-data`, field `file` (PDF, max 10MB).
- **Same for client-contract-format.**

---

# Part 6: Section Completion Rules

| Section | Required |
|---------|----------|
| Organization details | Stored in `brideside_vendors`: vendorName, contactNumber, emailId, (accountOwner or organization.owner), and at least one of officeStudioLocation or baseLocation (per vendor) |
| Asset info | At least one vendor asset per vendor |
| Events pricing | At least one event pricing row per vendor |
| Vendor data | At least one of master data link or calendar sheet link per vendor |
| Client data | Both quote format and client contract format PDFs |
| Team members | At least one team member per vendor |

---

# Part 7: Error Handling

| HTTP | Meaning |
|------|---------|
| 404 | Resource not found |
| 401 | Unauthorized |
| 403 | Forbidden |
| 400 | Bad request – validation error, or pipeline creation with inactive organization |

**Pipeline creation error:**
```json
{
  "success": false,
  "message": "Organization must be active (all onboarding details complete) to create a pipeline. Complete Organization details, Asset Info, Events Pricing, Vendor Data, Client Data, and Team Members."
}
```

---

# Part 8: Progress Bar Component Example

```tsx
function OrganizationProgressBar({ orgId }: { orgId: number }) {
  const { data: progress } = useQuery(
    ["organization-progress", orgId],
    () => getOrganizationProgress(orgId)
  );

  if (!progress) return null;

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
        <div style={{ width: `${(progress.completedCount / progress.totalCount) * 100}%` }} />
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
