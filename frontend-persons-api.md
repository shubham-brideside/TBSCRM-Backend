# Persons API (Leads)

This sheet covers everything the frontend needs to render the “Add person” modal and manage leads.

---

## Create Person

- **Method / URL:** `POST /api/persons`
- **Body:**
  ```json
  {
    "name": "Aditi Sharma",
    "organizationId": 17,
    "ownerId": 42,
    "phone": "+91 98765 43210",
    "email": "aditi@example.com",
    "instagramId": "@aditimua",
    "leadDate": "2025-11-12",
    "label": "BRIDAL_MAKEUP",
    "source": "INSTAGRAM"
  }
  ```
  - `name` (required) – plain text, max 255 chars.
  - `organizationId` (optional) – use the org picker (see Organizations API).
  - `ownerId` (optional) – must be an active SALES user (see Owners dropdown below).
  - `phone`, `email`, `instagramId` (optional) – free text.
  - `leadDate` (optional) – ISO date (`yyyy-MM-dd`). Defaults to today if omitted.
  - `label` (optional) – one of the label enum codes (see Label dropdown).
  - `source` (optional) – one of the source enum codes (see Source dropdown).

- **Response (201 Created):**
  ```json
  {
    "id": 105,
    "name": "Aditi Sharma",
    "organizationId": 17,
    "organizationName": "Dream Weddings",
    "ownerId": 42,
    "ownerDisplayName": "Rohan Gupta",
    "ownerEmail": "rohan@brideside.com",
    "phone": "+91 98765 43210",
    "email": "aditi@example.com",
    "instagramId": "@aditimua",
    "leadDate": "2025-11-12",
    "label": "BRIDAL_MAKEUP",
    "source": "INSTAGRAM",
    "createdAt": "2025-11-12T06:31:14Z",
    "updatedAt": "2025-11-12T06:31:14Z"
  }
  ```

## Update Person

- **Method / URL:** `PUT /api/persons/{id}`
- **Body:** same shape as create; supply only fields you want to overwrite.
- **Response:** updated `PersonDTO`.

## Delete Person

- **Method / URL:** `DELETE /api/persons/{id}`
- **Response:** `204 No Content`

## Bulk Delete

- **Method / URL:** `DELETE /api/persons?ids=12&ids=15`
- **Response:** count of deleted rows.

## Get Person / Summary

- `GET /api/persons/{id}` → full `PersonDTO`.
- `GET /api/persons/{id}/summary` → wraps `PersonDTO` plus `dealsCount`.

---

## List Persons

- **Method / URL:** `GET /api/persons`
- **Query params (all optional):**
  - `q`: search term (name, phone, email, Instagram, organization name, owner name).
  - `label`: enum code (e.g. `BRIDAL_MAKEUP`).
  - `source`: enum code (e.g. `INSTAGRAM`).
  - `organizationId`: numeric id.
  - `ownerId`: numeric id.
  - `leadFrom`, `leadTo`: ISO dates (`yyyy-MM-dd`) for lead date range.
  - Standard paging params (`page`, `size`, `sort`). Defaults to `sort=createdAt,DESC`.
- **Response:** Spring page of `PersonDTO`.

Example:
```
GET /api/persons?label=BRIDAL_MAKEUP&leadFrom=2025-11-01&leadTo=2025-11-30&page=0&size=25
```

---

## Dropdown Endpoints

### Owners
- **Method / URL:** `GET /api/persons/owners`
- **Response:**
  ```json
  [
    {
      "id": 42,
      "firstName": "Rohan",
      "lastName": "Gupta",
      "email": "rohan@brideside.com",
      "displayName": "Rohan Gupta"
    }
  ]
  ```
- Only active users with role `SALES` are returned. Use `displayName` as the select label and `id` for `ownerId`.

### Labels
- **Method / URL:** `GET /api/persons/labels`
- **Response:**
  ```json
  [
    { "code": "BRIDAL_MAKEUP", "label": "Bridal makeup" },
    { "code": "PARTY_MAKEUP", "label": "Party makeup" },
    { "code": "ENGAGEMENT", "label": "Engagement" },
    { "code": "RECEPTION", "label": "Reception" },
    { "code": "OTHER", "label": "Other" }
  ]
  ```

### Sources
- **Method / URL:** `GET /api/persons/sources`
- **Response:**
  ```json
  [
    { "code": "INSTAGRAM", "label": "Instagram" },
    { "code": "WHATSAPP", "label": "Whatsapp" },
    { "code": "TBS_WEBSITE", "label": "TBS Website" },
    { "code": "REFERRAL", "label": "Referral" },
    { "code": "OTHER", "label": "Other" }
  ]
  ```

---

## Field Notes

- `leadDate` defaults to the current date when omitted during creation.
- `organizationId` and `ownerId` are optional; omit them if the lead is unattached.
- Enumerations are case-insensitive when sent (backend uppercases before matching).
- API forces `name` and owner role validation—backend will reject requests without a name or with owners that are not active SALES users.

That’s everything required to wire up the “Add person” dialog and related views. Ping if you need example fetch code or testing tips. 

