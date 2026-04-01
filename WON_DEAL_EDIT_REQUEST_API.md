# Won Deal Edit Request API

This document explains how won deal editing works with admin approval.

## Flow Summary

1. User opens a WON deal and clicks **Edit Won Deal**.
2. User fills the deal form and submits edit request.
3. Backend stores request as `PENDING` in `deal_edit_requests` (deal is not updated yet).
4. Admin dashboard fetches pending requests and shows them as notifications.
5. Admin either:
   - Accepts request -> deal is updated, request status becomes `PERMITTED`.
   - Rejects request -> deal is not updated, request status becomes `REJECTED`.
6. Accepted/rejected requests no longer appear in pending notifications.

## Authentication and Roles

- User create request endpoint: authenticated users.
- Pending/accept/reject endpoints: admin role required.

---

## 1) Create Edit Request (User)

### Endpoint

- `POST /api/deals/{dealId}/edit-requests`

### Purpose

Create an edit request for a deal that is already `WON`.

### Request Body

```json
{
  "reason": "Client changed venue and increased budget",
  "changes": {
    "value": 150000,
    "venue": "New Venue Name",
    "city": "Mumbai",
    "notes": "Client confirmed updates on call"
  }
}
```

`changes` uses the same shape as `DealDtos.UpdateRequest` (all fields optional).

### Success Response

```json
{
  "success": true,
  "message": "Edit request created",
  "data": {
    "id": 10,
    "dealId": 1234,
    "currentDeal": {
      "id": 1234,
      "name": "Original Deal Name",
      "value": 100000,
      "status": "WON",
      "venue": "Old Venue"
    },
    "changes": {
      "value": 150000,
      "venue": "New Venue Name",
      "city": "Mumbai",
      "notes": "Client confirmed updates on call"
    },
    "reason": "Client changed venue and increased budget",
    "status": "PENDING",
    "requestedByUserId": 45,
    "requestedByName": "Sales User",
    "processedByUserId": null,
    "processedByName": null,
    "adminComment": null,
    "createdAt": "2026-03-31T11:00:00",
    "processedAt": null
  }
}
```

### Validation Notes

- Deal must exist.
- Deal status must be `WON`.
- `changes` is required.

---

## 2) List Pending Requests (Admin Dashboard Notifications)

### Endpoint

- `GET /api/deals/edit-requests/pending`

### Purpose

Get all pending won-deal edit requests for admin review.

### Success Response

```json
{
  "success": true,
  "message": "Pending edit requests fetched",
  "data": [
    {
      "id": 10,
      "dealId": 1234,
      "currentDeal": {
        "id": 1234,
        "name": "Original Deal Name",
        "value": 100000,
        "status": "WON",
        "venue": "Old Venue"
      },
      "changes": null,
      "reason": "Client changed venue and increased budget",
      "status": "PENDING",
      "requestedByUserId": 45,
      "requestedByName": "Sales User",
      "processedByUserId": null,
      "processedByName": null,
      "adminComment": null,
      "createdAt": "2026-03-31T11:00:00",
      "processedAt": null
    }
  ]
}
```

### UI Note

- Only `PENDING` requests are returned.
- Once accepted/rejected, request will not be in this list.

---

## 3) Accept Request (Admin)

### Endpoint

- `POST /api/deals/edit-requests/{requestId}/accept`

### Purpose

Approve request and apply requested changes to the actual deal.

### Optional Request Body

```json
{
  "adminComment": "Approved after verification"
}
```

### Backend Behavior

1. Request must be `PENDING`.
2. Stored `requestedChanges` JSON is converted to `DealDtos.UpdateRequest`.
3. Existing `dealService.update(dealId, changes)` is called.
4. Request status is updated to `PERMITTED`.

### Success Response

```json
{
  "success": true,
  "message": "Edit request accepted",
  "data": {
    "id": 10,
    "dealId": 1234,
    "currentDeal": {
      "id": 1234,
      "name": "Original Deal Name",
      "value": 150000,
      "status": "WON",
      "venue": "New Venue Name"
    },
    "changes": null,
    "reason": "Client changed venue and increased budget",
    "status": "PERMITTED",
    "requestedByUserId": 45,
    "requestedByName": "Sales User",
    "processedByUserId": 1,
    "processedByName": "Admin User",
    "adminComment": "Approved after verification",
    "createdAt": "2026-03-31T11:00:00",
    "processedAt": "2026-03-31T11:10:00"
  }
}
```

---

## 4) Reject Request (Admin)

### Endpoint

- `POST /api/deals/edit-requests/{requestId}/reject`

### Purpose

Reject request without modifying the deal.

### Optional Request Body

```json
{
  "adminComment": "Cannot modify after invoice generated"
}
```

### Backend Behavior

1. Request must be `PENDING`.
2. Deal is not changed.
3. Request status is updated to `REJECTED`.

### Success Response

```json
{
  "success": true,
  "message": "Edit request rejected",
  "data": {
    "id": 10,
    "dealId": 1234,
    "currentDeal": {
      "id": 1234,
      "name": "Original Deal Name",
      "value": 100000,
      "status": "WON",
      "venue": "Old Venue"
    },
    "changes": null,
    "reason": "Client changed venue and increased budget",
    "status": "REJECTED",
    "requestedByUserId": 45,
    "requestedByName": "Sales User",
    "processedByUserId": 1,
    "processedByName": "Admin User",
    "adminComment": "Cannot modify after invoice generated",
    "createdAt": "2026-03-31T11:00:00",
    "processedAt": "2026-03-31T11:05:00"
  }
}
```

---

## Frontend Integration Checklist

- User side:
  - Open form from won deal detail page.
  - Prefill from existing deal data.
  - Submit to `POST /api/deals/{dealId}/edit-requests` with `reason + changes`.
  - Show success message: request sent for approval.

- Admin side:
  - Show notification badge/list from `GET /api/deals/edit-requests/pending`.
  - Add Accept/Reject actions per request.
  - Call accept/reject endpoint.
  - Refresh pending list after action (request disappears).

---

## Status Values

- `PENDING` -> waiting for admin action.
- `PERMITTED` -> approved and deal updated.
- `REJECTED` -> denied; deal unchanged.

