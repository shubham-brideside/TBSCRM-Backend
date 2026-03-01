## Vendor Assets API (for ASSET INFO section)

Use these endpoints to fetch and edit vendor asset information (phone model, SIM card, issued by, issued on) for the **ASSET INFO** section on the Organization Details page.

All responses use the standard envelope:

```json
{
  "success": true|false,
  "message": "Human readable message",
  "data": { ... }
}
```

---

### Base URL

- `/api/organizations`

---

### Auto-creation

When an **organization** is created, the backend automatically creates:
1. A `brideside_vendors` entry
2. A `vendor_assets` entry (linked to that vendor and organization)

So each vendor has at least one asset row. Use the APIs below to **show** and **edit** it.

---

### List vendor assets (show details)

- **Method / URL:** `GET /api/organizations/{organizationId}/vendors/{vendorId}/assets`
- **Auth:** same as other secured endpoints (`Authorization: Bearer <token>`)

#### Response (200 OK)

```json
{
  "success": true,
  "message": "Vendor assets fetched",
  "data": [
    {
      "id": 1,
      "vendorId": 5,
      "organizationId": 44,
      "phoneModel": "iPhone 14",
      "phoneIssuedBy": "Brideside",
      "simCard": "Airtel 9876543210",
      "simIssuedBy": "Brideside",
      "issuedOn": "2026-01-15",
      "createdAt": "2026-03-01T10:00:00",
      "updatedAt": "2026-03-01T12:30:00"
    }
  ]
}
```

- **Note:** Typically one asset per vendor. Use `data[0]` for the ASSET INFO form.

---

### Update vendor asset (edit details)

- **Method / URL:** `PUT /api/organizations/{organizationId}/vendors/{vendorId}/assets/{assetId}`
- **Body:**

```json
{
  "phoneModel": "iPhone 14",
  "phoneIssuedBy": "Brideside",
  "simCard": "Airtel 9876543210",
  "simIssuedBy": "Brideside",
  "issuedOn": "2026-01-15"
}
```

- **Field notes:**
  - `issuedOn`: ISO date string (`YYYY-MM-DD`). Use `null` to clear.
  - All fields are optional; send only what you want to update.

#### Response (200 OK)

```json
{
  "success": true,
  "message": "Vendor asset updated",
  "data": {
    "id": 1,
    "vendorId": 5,
    "organizationId": 44,
    "phoneModel": "iPhone 14",
    "phoneIssuedBy": "Brideside",
    "simCard": "Airtel 9876543210",
    "simIssuedBy": "Brideside",
    "issuedOn": "2026-01-15",
    "createdAt": "2026-03-01T10:00:00",
    "updatedAt": "2026-03-01T12:35:00"
  }
}
```

---

### Suggested UI flow

1. Load org + vendors: `GET /api/organizations/{id}/with-details`
2. For the ASSET INFO section, use the first vendor’s id: `vendorId = data.vendors[0].id`
3. Load assets: `GET /api/organizations/{orgId}/vendors/{vendorId}/assets`
4. Display `data[0]` in the form (phone model, phone issued by, SIM card, SIM issued by, issued on)
5. On Save: `PUT /api/organizations/{orgId}/vendors/{vendorId}/assets/{assetId}` with the edited fields

---

### Frontend fetch example

```ts
async function fetchVendorAssets(orgId: number, vendorId: number, token: string) {
  const res = await fetch(`/api/organizations/${orgId}/vendors/${vendorId}/assets`, {
    method: "GET",
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${token}`,
    },
  });
  const body = await res.json();
  if (!res.ok || !body?.success) {
    throw new Error(body?.message || "Failed to fetch vendor assets");
  }
  return body.data as Array<{
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
  }>;
}

async function updateVendorAsset(
  orgId: number,
  vendorId: number,
  assetId: number,
  payload: {
    phoneModel?: string | null;
    phoneIssuedBy?: string | null;
    simCard?: string | null;
    simIssuedBy?: string | null;
    issuedOn?: string | null;
  },
  token: string
) {
  const res = await fetch(
    `/api/organizations/${orgId}/vendors/${vendorId}/assets/${assetId}`,
    {
      method: "PUT",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${token}`,
      },
      body: JSON.stringify(payload),
    }
  );
  const body = await res.json();
  if (!res.ok || !body?.success) {
    throw new Error(body?.message || "Failed to update vendor asset");
  }
  return body.data;
}
```
