## Organizations API Cheatsheet

Use these endpoints to power the Organizations admin/front-office screens. All endpoints return the standard envelope:

```json
{
  "success": true|false,
  "message": "Human readable message",
  "data": { ... } // optional
}
```

Validation failures respond with HTTP `400` and `success: false`. Missing records respond with HTTP `404` and message `"Organization not found with id {id}"`.

---

### Base URL

- `/api/organizations`

---

### Create organization

- **Method / URL:** `POST /api/organizations`
- **Body (`OrganizationRequest`):**
  ```json
  {
    "name": "Brideside Chicago",        // required, trimmed, max 255 chars
    "category": "PHOTOGRAPHY",          // required enum (PHOTOGRAPHY | MAKEUP | PLANNING_AND_DECOR)
    "ownerId": 5,                       // optional -> must be SALES or CATEGORY_MANAGER
    "address": "123 Lake Shore Dr ..."  // optional, max 500 chars
  }
  ```
- **Response (201 Created):**
  ```json
  {
    "success": true,
    "message": "Organization created",
    "data": {
      "id": 12,
      "name": "Brideside Chicago",
      "category": "PHOTOGRAPHY",
      "owner": {
        "id": 5,
        "firstName": "Jane",
        "lastName": "Doe",
        "email": "jane.doe@brideside.com",
        "role": "SALES",
        "displayName": "Jane Doe"
      },
      "address": "123 Lake Shore Dr, Chicago, IL",
      "createdAt": "2025-11-08T06:30:00Z",
      "updatedAt": "2025-11-08T06:30:00Z"
    }
  }
  ```
- **Frontend notes:**
  - Disable submit button while POST is in flight.
  - Trim user input before sending (backend trims as well).
  - Surface validation errors from the `message` field.

---

### List organizations

- **Method / URL:** `GET /api/organizations`
- **Response (200 OK):**
  ```json
  {
    "success": true,
    "message": "Organizations fetched",
    "data": [
      {
        "id": 12,
        "name": "Brideside Chicago",
        "category": "PHOTOGRAPHY",
        "owner": {
          "id": 5,
          "firstName": "Jane",
          "lastName": "Doe",
          "email": "jane.doe@brideside.com",
          "role": "SALES",
          "displayName": "Jane Doe"
        },
        "address": "123 Lake Shore Dr, Chicago, IL",
        "createdAt": "2025-11-08T06:30:00Z",
        "updatedAt": "2025-11-08T06:35:00Z"
      },
      {
        "id": 13,
        "name": "Brideside LA",
        "category": "MAKEUP",
        "owner": {
          "id": 8,
          "firstName": "John",
          "lastName": "Smith",
          "email": "john.smith@brideside.com",
          "role": "CATEGORY_MANAGER",
          "displayName": "John Smith"
        },
        "address": "456 Sunset Blvd, Los Angeles, CA",
        "createdAt": "2025-11-08T06:40:00Z",
        "updatedAt": "2025-11-08T06:40:00Z"
      }
    ]
  }
  ```
- **Frontend notes:**
  - Use to populate table/dropdowns.
  - Expect empty array when no records exist.
  - Cache locally if the list rarely changes; refresh after create/update/delete.

---

### Get single organization

- **Method / URL:** `GET /api/organizations/{id}`
- **Response (200 OK):**
  ```json
  {
    "success": true,
    "message": "Organization fetched",
    "data": {
      "id": 12,
      "name": "Brideside Chicago",
      "category": "PHOTOGRAPHY",
      "owner": {
        "id": 5,
        "firstName": "Jane",
        "lastName": "Doe",
        "email": "jane.doe@brideside.com",
        "role": "SALES",
        "displayName": "Jane Doe"
      },
      "address": "123 Lake Shore Dr, Chicago, IL",
      "createdAt": "2025-11-08T06:30:00Z",
      "updatedAt": "2025-11-08T06:30:00Z"
    }
  }
  ```
- **Frontend notes:**
  - Useful for detail/edit panes; handle 404 by redirecting or showing toast.

---

### Update organization

- **Method / URL:** `PUT /api/organizations/{id}`
- **Body (`OrganizationRequest`):**
  ```json
  {
    "name": "Brideside Downtown",
    "category": "PLANNING_AND_DECOR",
    "ownerId": 5,
    "address": "789 Michigan Ave, Chicago, IL"
  }
  ```
- **Response (200 OK):**
  ```json
  {
    "success": true,
    "message": "Organization updated",
    "data": {
      "id": 12,
      "name": "Brideside Downtown",
      "category": "PLANNING_AND_DECOR",
      "owner": {
        "id": 5,
        "firstName": "Jane",
        "lastName": "Doe",
        "email": "jane.doe@brideside.com",
        "role": "SALES",
        "displayName": "Jane Doe"
      },
      "address": "789 Michigan Ave, Chicago, IL",
      "createdAt": "2025-11-08T06:30:00Z",
      "updatedAt": "2025-11-08T07:00:00Z"
    }
  }
  ```
- **Frontend notes:**
  - Treat as replace operation: send the whole payload.
  - Optimistically update UI but handle rollback on error.

---

### Delete organization

- **Method / URL:** `DELETE /api/organizations/{id}`
- **Response (200 OK):**
  ```json
  {
    "success": true,
    "message": "Organization deleted",
    "data": null
  }
  ```
- **Frontend notes:**
  - Confirm with user before calling.
  - After success, remove row locally and refetch list if needed.

---

### Owner options (for dropdown)

- **Method / URL:** `GET /api/organizations/owners`
- **Response (200 OK):**
  ```json
  {
    "success": true,
    "message": "Organization owners fetched",
    "data": [
      {
        "id": 5,
        "firstName": "Jane",
        "lastName": "Doe",
        "email": "jane.doe@brideside.com",
        "role": "SALES",
        "displayName": "Jane Doe"
      },
      {
        "id": 8,
        "firstName": "John",
        "lastName": "Smith",
        "email": "john.smith@brideside.com",
        "role": "CATEGORY_MANAGER",
        "displayName": "John Smith"
      }
    ]
  }
  ```
- **Frontend notes:**
  - Call once on form load and reuse results; dataset is small.
  - Use `displayName` (or build it from first/last) for the option label.
  - Save the selected `id` as `ownerId` when submitting.

---

### Category options (for dropdown)

- **Method / URL:** `GET /api/organizations/categories`
- **Response (200 OK):**
  ```json
  {
    "success": true,
    "message": "Organization categories fetched",
    "data": [
      { "code": "PHOTOGRAPHY", "label": "Photography" },
      { "code": "MAKEUP", "label": "Makeup" },
      { "code": "PLANNING_AND_DECOR", "label": "Planning and Decor" }
    ]
  }
  ```
- **Frontend notes:**
  - Use `label` for rendering and `code` when submitting or editing.
  - Categories are static; cache locally or keep in app constants once fetched.

---

### Integration Tips

1. **Authentication:** use existing auth headers/tokens.
2. **Form handling:** enforce max lengths (`name` 255, `address` 500) and trim inputs; `ownerId` must be a positive number.
3. **Category validation:** populate dropdown from `/api/organizations/categories` and submit the `code`.
4. **Owner validation:** disable selection for inactive users; backend rejects inactive/invalid roles.
5. **Empty states:** show friendly message when `data` array is empty.
6. **Toasts/snacks:** surface the backend `message` to users; 404/400 payloads are human-readable.


