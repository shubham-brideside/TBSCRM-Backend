## Users API Cheatsheet

All responses use the standard envelope:

```json
{
  "success": true|false,
  "message": "Human readable message",
  "data": { ... } // optional
}
```

Validation errors respond with HTTP `400`; missing resources return HTTP `404`.

---

### Create user

- **Method / URL:** `POST /api/users`
- **Body (`CreateUserRequest`):**
  ```json
  {
    "email": "alex@brideside.com",
    "firstName": "Alex",
    "lastName": "Morgan",
    "role": "SALES",
    "managerId": 18        // optional; must follow manager rules below
  }
  ```
- **Manager rules:**
  - `ADMIN`, `CATEGORY_MANAGER` → managerId must be omitted.
  - `SALES` → manager must be a `CATEGORY_MANAGER`.
  - `PRESALES` → manager must be a `SALES`.
- On success the user is created inactive and an invitation email is sent.

---

### Manager options (for dropdown)

- **Method / URL:** `GET /api/users/managers?forRole={role}`
- **Query param (`forRole`):** one of `ADMIN`, `CATEGORY_MANAGER`, `SALES`, `PRESALES`.
- **Response (200 OK):**
  ```json
  {
    "success": true,
    "message": "Managers fetched",
    "data": [
      {
        "id": 18,
        "firstName": "Priya",
        "lastName": "Singh",
        "email": "priya.singh@brideside.com",
        "role": "CATEGORY_MANAGER",
        "displayName": "Priya Singh"
      }
    ]
  }
  ```
- **Behavior:**
  - `ADMIN` / `CATEGORY_MANAGER` → empty array (manager field should be disabled).
  - `SALES` → list of active `CATEGORY_MANAGER` users.
  - `PRESALES` → list of active `SALES` users.

---

### Update user

- **Method / URL:** `PUT /api/users/{userId}`
- **Body:** same structure as create; omitting `managerId` keeps the existing manager if still valid.
- Manager validation follows the same rules described above.
- **Response (200 OK):** updated `UserResponse`.

---

### List users

- **Method / URL:** `GET /api/users`
- **Behavior:** respects role hierarchy (Admin sees all, Category Manager sees their tree, etc.).
- **Response (200 OK):** array of `UserResponse`.

---

### Get user

- **Method / URL:** `GET /api/users/{userId}`
- **Response (200 OK):** `UserResponse` for the requested user.

---

### Delete user

- **Method / URL:** `DELETE /api/users/{userId}?reassignManagerId={id}`
- Requires `ADMIN` role. If the user has subordinates, provide `reassignManagerId` to reassign them; otherwise deletion is blocked.

---

### `UserResponse` reference

```json
{
  "id": 44,
  "email": "alex@brideside.com",
  "firstName": "Alex",
  "lastName": "Morgan",
  "role": "SALES",
  "active": false,
  "passwordSet": false,
  "managerId": 18,
  "managerName": "Priya Singh",
  "createdAt": "2025-11-10T09:42:10",
  "lastLoginAt": null
}
```

---

### Integration tips

1. **Role dropdown:** determine the selected role first, then call `/api/users/managers?forRole=<ROLE>` to populate the manager picker (or disable it if the response is empty).
2. **User picker for manager reassignment:** reuse the same endpoint as above.
3. **Invitation flow:** new users are inactive until they set their password via the invitation email.
4. **Hierarchy enforcement:** backend prevents circular manager chains; handle validation messages surfaced in `message`.

