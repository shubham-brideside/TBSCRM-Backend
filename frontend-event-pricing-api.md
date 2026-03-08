# Events Pricing API – Frontend Integration

API for managing event/resource pricing per vendor. Supports different layouts by organization category:

- **Photography / Planning / Decor**: Resource-based, single session, primary only (Cost Per Day, Notes/Policy)
- **Makeup**: Event-based, Current & Upcoming sessions, Primary/Senior/Junior artist levels

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

## Get Event Pricing

Event pricing is included in vendor responses:

- `GET /api/organizations/{organizationId}/vendors`
- `GET /api/organizations/{organizationId}/with-details`

Each vendor has:

- `eventPricing` – Photography etc. (single session, primary only)
- `eventPricingCurrent` – Makeup Current session (Jan–Sep)
- `eventPricingUpcoming` – Makeup Upcoming session (Oct–next Sep)

### Example: Photography vendor response

```json
{
  "id": 33,
  "vendorName": "Photo Studio",
  "eventPricing": [
    {
      "code": "Candid Photographer",
      "label": "Candid Photographer",
      "primary": {
        "basePrice": 50000,
        "destinationPrice": null,
        "additionalMakeupPrice": null,
        "availabilityAtStudio": null,
        "policyNotes": "Full day coverage. Raw files on request.",
        "currency": "INR"
      },
      "senior": null,
      "junior": null
    }
  ],
  "eventPricingCurrent": [],
  "eventPricingUpcoming": []
}
```

### Example: Makeup vendor response

```json
{
  "id": 34,
  "vendorName": "Makeup Artist",
  "eventPricing": [],
  "eventPricingCurrent": [
    {
      "code": "Wedding",
      "label": "Wedding",
      "primary": {
        "basePrice": 75000,
        "destinationPrice": 95000,
        "additionalMakeupPrice": 8000,
        "availabilityAtStudio": "Mon–Sat, 9 AM–6 PM",
        "policyNotes": "50% advance. Cancellation 7 days prior.",
        "currency": "INR"
      },
      "senior": {
        "basePrice": 55000,
        "destinationPrice": 70000,
        "additionalMakeupPrice": 6000,
        "availabilityAtStudio": null,
        "policyNotes": null,
        "currency": "INR"
      },
      "junior": {
        "basePrice": 35000,
        "destinationPrice": 45000,
        "additionalMakeupPrice": 4000,
        "availabilityAtStudio": null,
        "policyNotes": null,
        "currency": "INR"
      }
    }
  ],
  "eventPricingUpcoming": []
}
```

---

## Save Event Pricing

- **Method / URL:** `PUT /api/organizations/{organizationId}/vendors/{vendorId}/event-pricing`
- **Auth:** `Authorization: Bearer <token>`

Replaces all event pricing for the vendor. Send only the sections relevant to the org category.

### Photography (resource-based, primary only)

```json
{
  "eventPricing": [
    {
      "code": "Candid Photographer",
      "label": "Candid Photographer",
      "primary": {
        "basePrice": 50000,
        "destinationPrice": null,
        "additionalMakeupPrice": null,
        "availabilityAtStudio": null,
        "policyNotes": "Full day coverage. Raw files on request.",
        "currency": "INR"
      },
      "senior": null,
      "junior": null
    },
    {
      "code": "Cinematographer",
      "label": "Cinematographer",
      "primary": {
        "basePrice": 60000,
        "policyNotes": "8-hour coverage",
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

### Makeup (event-based, Current & Upcoming, artist levels)

```json
{
  "eventPricing": null,
  "eventPricingCurrent": [
    {
      "code": "Roka",
      "label": "Roka",
      "primary": {
        "basePrice": 30000,
        "destinationPrice": 45000,
        "additionalMakeupPrice": 5000,
        "availabilityAtStudio": "Mon–Fri",
        "policyNotes": "Advance booking required",
        "currency": "INR"
      },
      "senior": {
        "basePrice": 25000,
        "destinationPrice": 38000,
        "additionalMakeupPrice": 6000,
        "currency": "INR"
      },
      "junior": {
        "basePrice": 15000,
        "destinationPrice": 22000,
        "additionalMakeupPrice": 4000,
        "currency": "INR"
      }
    }
  ],
  "eventPricingUpcoming": [
    {
      "code": "Roka",
      "label": "Roka",
      "primary": {
        "basePrice": 32000,
        "destinationPrice": 48000,
        "currency": "INR"
      },
      "senior": null,
      "junior": null
    }
  ]
}
```

### Response (200 OK)

Returns full `VendorResponse` with updated `eventPricing`, `eventPricingCurrent`, `eventPricingUpcoming`.

---

## Field Reference

| Field | Type | Description |
|-------|------|-------------|
| `code` | string | Short identifier (e.g. "Roka", "Candid Photographer") |
| `label` | string | Display label (usually same as code) |
| `primary` | object | Primary artist pricing |
| `senior` | object | Senior artist pricing (Makeup only) |
| `junior` | object | Junior artist pricing (Makeup only) |

### Artist pricing object

| Field | Type | Description |
|-------|------|-------------|
| `basePrice` | number | Base price / cost per day |
| `destinationPrice` | number | Outstation/destination price |
| `additionalMakeupPrice` | number | Extra party makeup (Makeup only) |
| `availabilityAtStudio` | string | Free text |
| `policyNotes` | string | Free text |
| `currency` | string | e.g. "INR" |

---

## Add / Edit / Delete

- **Add:** Include a new row in the array and send the full payload.
- **Edit:** Change the row and send the full payload.
- **Delete:** Remove the row from the array and send the full payload.

The backend replaces all pricing for the vendor on each save.

---

## Error Responses

| HTTP | Condition |
|------|-----------|
| 404 | Organization not found |
| 404 | Vendor not found for organization |
| 401/403 | Auth/permissions |

---

## Frontend Integration Example

```ts
async function saveEventPricing(
  organizationId: number,
  vendorId: number,
  payload: {
    eventPricing?: EventPricingRow[] | null;
    eventPricingCurrent?: EventPricingRow[] | null;
    eventPricingUpcoming?: EventPricingRow[] | null;
  },
  token: string
) {
  const res = await fetch(
    `/api/organizations/${organizationId}/vendors/${vendorId}/event-pricing`,
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
    throw new Error(body?.message || "Failed to save event pricing");
  }
  return body.data;
}
```

---

## Summary

| Action | Method | Endpoint |
|--------|--------|----------|
| Get event pricing | `GET` | `/api/organizations/{orgId}/vendors` or `.../with-details` |
| Save event pricing | `PUT` | `/api/organizations/{orgId}/vendors/{vendorId}/event-pricing` |
