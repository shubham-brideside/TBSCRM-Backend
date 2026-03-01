## Brideside Vendors API (for Organization Details page)

Use this endpoint to fetch vendor-specific fields for a **single organization** (based on `organizationId`) from the `brideside_vendors` table.

All responses use the standard envelope:

```json
{
  "success": true|false,
  "message": "Human readable message",
  "data": { ... } // optional
}
```

Errors:

- HTTP `404`: organization not found → `{"success":false,"message":"Organization not found with id {id}","data":null}`
- HTTP `401/403`: auth/permissions issues (use the same auth flow as other `/api/organizations` endpoints)

---

### Base URL

- `/api/organizations`

---

### List vendors for an organization

- **Method / URL:** `GET /api/organizations/{organizationId}/vendors`
- **Auth:** same as other secured endpoints (send `Authorization: Bearer <token>`)
- **Query params:** none

#### Response (200 OK)

```json
{
  "success": true,
  "message": "Organization vendors fetched",
  "data": [
    {
      "id": 33,
      "username": "vendor_username",
      "organizationId": 12,
      "pipelineId": 7,
      "igAccountId": "17841400000000000",
      "businessName": "Vendor Business Name",
      "vendorName": "Vendor Display Name",
      "services": "[\"Photography\",\"Candid\"]",
      "accountOwner": {
        "id": 5,
        "firstName": "Jane",
        "lastName": "Doe",
        "email": "jane.doe@brideside.com"
      },

      "contactNumber": "+91-9876543210",
      "officeStudioLocation": "Koramangala, Bangalore",
      "baseLocation": "Bangalore",
      "officialNumber": "+91-8040000000",
      "emailId": "vendor@company.com",
      "onboardingDate": "2026-03-01T10:30:00",
      "teamSize": 6,
      "onboardingFee": 25000.00,

      "createdAt": "2026-02-01T12:00:00",
      "updatedAt": "2026-03-01T09:15:00"
    }
  ]
}
```

---

### Create vendor entry for an organization (when list is empty)

When a new organization is created, it may not have a row in `brideside_vendors` yet. To let users enter vendor details, create a vendor entry first.

- **Method / URL:** `POST /api/organizations/{organizationId}/vendors`
- **Body:** optional (send `{}` if you don’t have anything yet). If `pipelineId` is omitted and the organization has no pipelines, the backend creates a **default pipeline** automatically.

```json
{
  "username": "vendor_username",      // optional; if omitted backend generates "vendor-org-{orgId}"
  "pipelineId": 45,                   // optional
  "igAccountId": "1784140...",        // optional
  "businessName": "RSP",              // optional
  "services": "[\"Photography\"]",    // optional (JSON string)

  "contactNumber": "+91-9876543210",
  "officeStudioLocation": "Koramangala, Bangalore",
  "baseLocation": "Bangalore",
  "officialNumber": "+91-8040000000",
  "emailId": "vendor@company.com",
  "onboardingDate": "2026-03-01T10:30:00",
  "teamSize": 6,
  "onboardingFee": 25000.00
}
```

- **Response (201 Created):** `VendorResponse` (same shape as list item)

---

### Get organization with vendor details (single call)

- **Method / URL:** `GET /api/organizations/{organizationId}/with-details`
- **Response (200 OK):**
  - `data.organization`: same shape as `GET /api/organizations/{id}`
  - `data.vendors`: same shape as `GET /api/organizations/{id}/vendors`

---

### Update vendor details for an organization

- **Method / URL:** `PUT /api/organizations/{organizationId}/vendors/{vendorId}`
- **Body:**

```json
{
  "pipelineId": 45,
  "vendorName": "Vendor Display Name",
  "contactNumber": "+91-9876543210",
  "officeStudioLocation": "Koramangala, Bangalore",
  "baseLocation": "Bangalore",
  "officialNumber": "+91-8040000000",
  "emailId": "vendor@company.com",
  "onboardingDate": "2026-03-01T10:30:00",
  "teamSize": 6,
  "onboardingFee": 25000.00
}
```

- **Notes:**
  - Treat this as a **replace** of these fields: send `null` to clear a value.
  - `pipelineId` is optional. Use it later when you create pipelines for the org.
  - `access_token` is not accepted/exposed.

---

### Field notes / UI mapping

- **`services`**: returned as a JSON string (because DB column is `JSON`). If your UI expects an array, parse it:
  - `JSON.parse(vendor.services)` → array/object (wrap in try/catch; it can be `null`)
- **`accountOwner`**: comes from `brideside_vendors.account_owner` → `users` table (nullable; field may be `null`)
- **`onboardingFee`**: decimal number; render as currency
- **`onboardingDate` / `createdAt` / `updatedAt`**: returned as ISO-like datetime strings; format for display in UI

---

### Security note

- The backend **does not expose** `access_token` in this API response. Do not rely on it being present.

---

### Frontend integration example (fetch)

```ts
// Example: React/Next/SPA fetch (adjust base URL to your setup)
async function fetchOrganizationVendors(organizationId: number, token: string) {
  const res = await fetch(`/api/organizations/${organizationId}/vendors`, {
    method: "GET",
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${token}`,
    },
  });

  const body = await res.json();
  if (!res.ok || !body?.success) {
    throw new Error(body?.message || "Failed to fetch organization vendors");
  }

  return body.data as Array<{
    id: number;
    username: string;
    organizationId: number;
    pipelineId: number;
    igAccountId: string | null;
    businessName: string | null;
    vendorName: string | null;
    services: string | null;
    accountOwner: null | { id: number; firstName: string; lastName: string; email: string };
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
  }>;
}
```

---

### Suggested UI behavior

1. Call `GET /api/organizations/{id}` for organization core details.
2. Call `GET /api/organizations/{id}/vendors` for vendor-related details (this doc).
3. If vendors list is empty and the user clicks **Edit**, call `POST /api/organizations/{id}/vendors` (send `{}`), then refetch vendors and show the form.

