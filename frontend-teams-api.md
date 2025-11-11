## Teams API Cheatsheet

All responses return the standard envelope:

```json
{
  "success": true|false,
  "message": "Human readable message",
  "data": { ... } // optional
}
```

Validation failures respond with HTTP `400` and `success: false`. Missing resources return HTTP `404`.

---

### Create team

- **Method / URL:** `POST /api/teams`
- **Body (`TeamRequest`):**
  ```json
  {
    "name": "North America Sales",   // required, trimmed, max 255 chars
    "managerId": 12,                 // optional, must reference an active SALES user
    "memberIds": [44, 45, 52]        // optional, active PRESALES users only; duplicates ignored
  }
  ```
- **Response (201 Created):**
  ```json
  {
    "success": true,
    "message": "Team created",
    "data": {
      "id": 7,
      "name": "North America Sales",
      "manager": {
        "id": 12,
        "firstName": "Jane",
        "lastName": "Doe",
        "email": "jane.doe@brideside.com",
        "role": "CATEGORY_MANAGER",
        "displayName": "Jane Doe"
      },
      "members": [
        { "id": 12, "firstName": "Jane", "lastName": "Doe", "email": "...", "role": "CATEGORY_MANAGER" },
        { "id": 18, "firstName": "Miguel", "lastName": "Ramos", "email": "...", "role": "SALES" }
      ],
      "createdAt": "2025-11-10T09:42:10Z",
      "updatedAt": "2025-11-10T09:42:10Z"
    }
  }
  ```

---

### List teams

- **Method / URL:** `GET /api/teams`
- **Response (200 OK):** array of `TeamResponse` objects sorted by name.

---

### Get team

- **Method / URL:** `GET /api/teams/{teamId}`
- **Response (200 OK):** single `TeamResponse`.

---

### Update team

- **Method / URL:** `PUT /api/teams/{teamId}`
- **Body:** same structure as create. Include `memberIds` (can be empty) to replace the membership.
- **Optional field:** `clearManager: true` to remove the current manager without assigning a new one.
- **Response (200 OK):** updated `TeamResponse`.

---

### Delete team

- **Method / URL:** `DELETE /api/teams/{teamId}`
- **Behavior:** Teams can be deleted only when no pipelines reference them.
- **Response:** `{ "success": true, "message": "Team deleted" }`

---

### Manager options (for dropdown)

- **Method / URL:** `GET /api/teams/managers?forRole={role}`
- **Response (200 OK):**
  ```json
  {
    "success": true,
    "message": "Team managers fetched",
    "data": [
      {
        "id": 12,
        "firstName": "Jane",
        "lastName": "Doe",
        "email": "jane.doe@brideside.com",
        "role": "SALES",
        "displayName": "Jane Doe"
      }
    ]
  }
  ```
- **Frontend notes:**
  - Pass `forRole` using one of `ADMIN`, `CATEGORY_MANAGER`, `SALES`, `PRESALES` to tailor the dropdown:
    - `ADMIN`, `CATEGORY_MANAGER` ⇒ returns empty list (manager not required).
    - `SALES` ⇒ returns active SALES users.
    - `PRESALES` ⇒ returns active SALES users.
  - Use `displayName` for the select label and `id` for `managerId`.

---

### Member options (for dropdown)

- **Method / URL:** `GET /api/teams/members`
- **Response (200 OK):**
  ```json
  {
    "success": true,
    "message": "Team members fetched",
    "data": [
      {
        "id": 44,
        "firstName": "Priya",
        "lastName": "Singh",
        "email": "priya.singh@brideside.com",
        "role": "PRESALES",
        "displayName": "Priya Singh"
      }
    ]
  }
  ```
- **Frontend notes:**
  - Only active PRESALES users are returned; backend enforces the role requirement.
  - Use `displayName` for labels and send `id` in `memberIds`.

### `TeamResponse` reference

```json
{
  "id": 7,
  "name": "North America Sales",
  "manager": {
    "id": 12,
    "firstName": "Jane",
    "lastName": "Doe",
    "email": "jane.doe@brideside.com",
    "role": "CATEGORY_MANAGER",
    "displayName": "Jane Doe"
  },
  "members": [
    {
      "id": 12,
      "firstName": "Jane",
      "lastName": "Doe",
      "email": "jane.doe@brideside.com",
      "role": "CATEGORY_MANAGER",
      "displayName": "Jane Doe"
    },
    {
      "id": 18,
      "firstName": "Miguel",
      "lastName": "Ramos",
      "email": "miguel.ramos@brideside.com",
      "role": "SALES",
      "displayName": "Miguel Ramos"
    }
  ],
  "createdAt": "2025-11-10T09:42:10Z",
  "updatedAt": "2025-11-10T09:50:32Z"
}
```

---

### Integration tips

1. **Authentication:** reuse existing JWT auth headers.
2. **Manager selection:** call `/api/teams/managers?forRole=<ROLE>` to fetch eligible SALES managers (or an empty list when managers aren’t required).
3. **Member selection:** use `/api/teams/members` to fetch the active PRESALES pool; this API expects user ids only.
4. **Membership updates:** send the full list of member ids each time—you replace the existing set.
5. **Manager vs members:** the manager does **not** have to appear in `memberIds`; include them if you want them listed in the roster.
6. **Clearing manager:** send `clearManager: true` (and omit `managerId`) during update to remove the current manager.
7. **Deletion guard:** pipelines that reference a team will block deletion. Update or remove those pipelines first.

