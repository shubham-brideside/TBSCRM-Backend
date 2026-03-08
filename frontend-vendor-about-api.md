# Vendor About API – Frontend Integration

API for adding and editing the **about** field (details of vendor services) for Brideside vendors.

---

## Base URL

`/api/organizations`

---

## Response Envelope

All responses use the standard envelope:

```json
{
  "success": true,
  "message": "Human readable message",
  "data": { ... }
}
```

---

## Add About (Create Vendor)

When creating a new vendor, include `about` in the request body.

- **Method / URL:** `POST /api/organizations/{organizationId}/vendors`
- **Auth:** `Authorization: Bearer <token>`

### Request Body

```json
{
  "username": "vendor_username",
  "pipelineId": 45,
  "businessName": "RSP",
  "vendorName": "Vendor Display Name",
  "about": "We specialize in wedding photography and candid shots. Our team has 10+ years of experience in destination weddings across India.",
  "services": "[\"Photography\",\"Candid\"]",
  "contactNumber": "+91-9876543210",
  "emailId": "vendor@company.com"
}
```

- **`about`** (optional): Details of vendor services. Plain text. Can be long (TEXT in DB).
- Other fields are optional; send `{}` for minimal creation.

### Response (201 Created)

Returns full `VendorResponse` including `about`:

```json
{
  "success": true,
  "message": "Organization vendor created",
  "data": {
    "id": 33,
    "username": "vendor_username",
    "organizationId": 12,
    "pipelineId": 45,
    "vendorName": "Vendor Display Name",
    "about": "We specialize in wedding photography and candid shots. Our team has 10+ years of experience in destination weddings across India.",
    "services": "[\"Photography\",\"Candid\"]",
    "contactNumber": "+91-9876543210",
    "emailId": "vendor@company.com",
    "createdAt": "2026-03-01T12:00:00",
    "updatedAt": "2026-03-01T12:00:00"
  }
}
```

---

## Edit About (Update Vendor – Full)

Use the full vendor update endpoint when you are editing multiple fields including `about`.

- **Method / URL:** `PUT /api/organizations/{organizationId}/vendors/{vendorId}`
- **Auth:** `Authorization: Bearer <token>`

### Request Body

```json
{
  "pipelineId": 45,
  "vendorName": "Vendor Display Name",
  "about": "Updated description of vendor services. We now offer videography as well.",
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

- **`about`** (optional): Details of vendor services. Send `null` to clear.
- All fields are optional; omit or send `null` to clear.

### Response (200 OK)

Returns full `VendorResponse` with updated `about`.

---

## Edit About (Update Vendor – About Only)

Use this endpoint when you only want to update the `about` field (e.g. dedicated About section).

- **Method / URL:** `PATCH /api/organizations/{organizationId}/vendors/{vendorId}/about`
- **Auth:** `Authorization: Bearer <token>`

### Request Body

```json
{
  "about": "We specialize in wedding photography, candid shots, and videography. 10+ years of experience in destination weddings across India."
}
```

**Note:** Only `about` is used; other fields in the body are ignored. Send `{}` or `{"about": null}` to clear.

### Response (200 OK)

```json
{
  "success": true,
  "message": "Vendor about updated",
  "data": {
    "id": 33,
    "username": "vendor_username",
    "organizationId": 12,
    "pipelineId": 45,
    "vendorName": "Vendor Display Name",
    "about": "We specialize in wedding photography, candid shots, and videography. 10+ years of experience in destination weddings across India.",
    "services": "[\"Photography\",\"Candid\"]",
    "contactNumber": "+91-9876543210",
    "emailId": "vendor@company.com",
    "createdAt": "2026-03-01T12:00:00",
    "updatedAt": "2026-03-07T14:30:00"
  }
}
```

---

## Get About (Read Vendor)

The `about` field is included in all vendor responses:

- `GET /api/organizations/{organizationId}/vendors` – list vendors
- `GET /api/organizations/{organizationId}/with-details` – organization with vendors

Each vendor object has an `about` field (string or `null`).

---

## Error Responses

| HTTP | Condition |
|------|-----------|
| 404 | Organization not found |
| 404 | Vendor not found for organization |
| 401/403 | Auth/permissions |

---

## Frontend Integration Examples

### TypeScript Types

```ts
interface VendorAboutResponse {
  id: number;
  username: string;
  organizationId: number;
  pipelineId: number;
  vendorName: string | null;
  about: string | null;
  services: string | null;
  contactNumber: string | null;
  emailId: string | null;
  createdAt: string;
  updatedAt: string;
  // ... other fields
}

interface ApiResponse<T> {
  success: boolean;
  message: string;
  data: T;
}
```

### Add About (Create Vendor)

```ts
async function createVendorWithAbout(
  organizationId: number,
  about: string,
  token: string
) {
  const res = await fetch(`/api/organizations/${organizationId}/vendors`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${token}`,
    },
    body: JSON.stringify({ about }),
  });

  const body = await res.json();
  if (!res.ok || !body?.success) {
    throw new Error(body?.message || "Failed to create vendor");
  }
  return body.data as VendorAboutResponse;
}
```

### Edit About (PATCH – About Only)

```ts
async function updateVendorAbout(
  organizationId: number,
  vendorId: number,
  about: string | null,
  token: string
) {
  const res = await fetch(
    `/api/organizations/${organizationId}/vendors/${vendorId}/about`,
    {
      method: "PATCH",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${token}`,
      },
      body: JSON.stringify({ about }),
    }
  );

  const body = await res.json();
  if (!res.ok || !body?.success) {
    throw new Error(body?.message || "Failed to update vendor about");
  }
  return body.data as VendorAboutResponse;
}
```

### Edit About (PUT – Full Vendor Update)

```ts
async function updateVendorAboutViaPut(
  organizationId: number,
  vendorId: number,
  vendor: { about?: string | null; vendorName?: string; /* ... */ },
  token: string
) {
  const res = await fetch(
    `/api/organizations/${organizationId}/vendors/${vendorId}`,
    {
      method: "PUT",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${token}`,
      },
      body: JSON.stringify(vendor),
    }
  );

  const body = await res.json();
  if (!res.ok || !body?.success) {
    throw new Error(body?.message || "Failed to update vendor");
  }
  return body.data as VendorAboutResponse;
}
```

### React Example (About Section)

```tsx
function VendorAboutSection({
  organizationId,
  vendorId,
  initialAbout,
  token,
}: {
  organizationId: number;
  vendorId: number;
  initialAbout: string | null;
  token: string;
}) {
  const [about, setAbout] = useState(initialAbout ?? "");
  const [saving, setSaving] = useState(false);

  const handleSave = async () => {
    setSaving(true);
    try {
      await updateVendorAbout(
        organizationId,
        vendorId,
        about.trim() || null,
        token
      );
      // Show success toast, etc.
    } catch (e) {
      // Show error
    } finally {
      setSaving(false);
    }
  };

  return (
    <div>
      <label>About (vendor services details)</label>
      <textarea
        value={about}
        onChange={(e) => setAbout(e.target.value)}
        rows={5}
        placeholder="Describe your services..."
      />
      <button onClick={handleSave} disabled={saving}>
        {saving ? "Saving..." : "Save About"}
      </button>
    </div>
  );
}
```

---

## Summary

| Action | Method | Endpoint |
|--------|--------|----------|
| Add about | `POST` | `/api/organizations/{orgId}/vendors` (include `about` in body) |
| Edit about | `PATCH` | `/api/organizations/{orgId}/vendors/{vendorId}/about` |
| Edit about | `PUT` | `/api/organizations/{orgId}/vendors/{vendorId}` (include `about` in body) |
| Get about | `GET` | `/api/organizations/{orgId}/vendors` or `.../with-details` |
