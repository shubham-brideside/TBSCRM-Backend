# Frontend changes: TBS user creation flow

This document describes what the **frontend** should send and show when creating users, now that the backend supports a separate **TBS** path alongside the existing **Acceltancy** (non-TBS) flow.

## API: `POST /api/users` (Admin only)

### Acceltancy users (unchanged)

- Omit `isTbsUser` or send `isTbsUser: false`.
- Send `role` as before: `ADMIN`, `CATEGORY_MANAGER`, `SALES`, or `PRESALES`, plus existing fields (`managerId`, `managedCategoryId` when applicable).

### TBS users

1. Send **`isTbsUser: true`**.
2. Send **`role`** as exactly one of (ENUM-style identifiers, aligned with existing roles like `PRESALES`):
   - `TBS_PRESALES` — TBS pre-sales  
   - `TBS_REL_MANAGER` — TBS relationship manager  
   - `TBS_SVC_MANAGER` — TBS service manager  
3. **Do not** send `managerId` for TBS users (managers are not used in this flow).
4. **Organization**
   - For `TBS_PRESALES` and `TBS_REL_MANAGER`: **do not** send `tbsOrganizationId`. The backend assigns **TBS Test** (default organization id **117**, overridable via env — see below).
   - For `TBS_SVC_MANAGER`: send **`tbsOrganizationId`** as one of:
     - **64** — Revaah  
     - **118** — TBS Planning  
     - **119** — TBS Venue  
     These ids default from `application.yml` / environment variables; the UI should load allowed options from config or hard-code the same defaults until a dedicated “TBS org options” API exists.

### Validation rules (mirror in the UI)

| `isTbsUser` | `role`                          | `tbsOrganizationId`     |
|-------------|---------------------------------|-------------------------|
| `false`/omit | Non-TBS role only             | Ignored                  |
| `true`      | Must be one of the three TBS_* | Required only for `TBS_SVC_MANAGER` |

If `isTbsUser` is `true` but `role` is not a `TBS_*` role, or the opposite, the API returns **400** with an explanatory message.

### Response fields (`UserResponse`)

New optional fields (populated for TBS users after create):

- `isTbsUser` — boolean  
- `tbsHomeOrganizationId` — organization used for that user’s TBS pipeline  
- `tbsDefaultPipelineId` — pipeline created with the user’s display name and role-specific stages  

The frontend can show these on user detail or admin confirmation screens.

## UX recommendations

1. **Toggle or wizard step**  
   - Step 1: “User type” → **Acceltancy** vs **TBS**.  
   - If **Acceltancy**, keep the current create-user form.  
   - If **TBS**, show only TBS role picker and conditional org picker.

2. **TBS role → organization UI**  
   - **Pre-Sales** / **Relationship Manager**: show read-only text: “Organization: TBS Test (auto-assigned)”.  
   - **Service Manager**: show a **select** with three options (labels + ids above).

3. **Pipeline**  
   - No extra user input: the backend creates one pipeline named like the user’s **display name** (first + last, or email if names blank) and attaches the correct stages.  
   - If that pipeline name already exists globally, the backend retries with `"Display Name (email@...)"`.

4. **`GET /api/users/managers?forRole=...`**  
   - Eligible managers for TBS roles return **empty**; hide the manager dropdown when a TBS role is selected.

5. **After invite**  
   - Same invitation / set-password flow as today; no change.

## Configuration (for frontend env parity)

Backend reads (defaults in parentheses):

| Variable                         | Purpose                          | Default |
|----------------------------------|----------------------------------|--------:|
| `TBS_USER_ONBOARD_ORG_TEST_ID`   | TBS Test (Pre-Sales / RM)        | 117     |
| `TBS_USER_ONBOARD_ORG_REVAAH_ID` | Service Manager option           | 64      |
| `TBS_USER_ONBOARD_ORG_PLANNING_ID` | Service Manager option         | 118     |
| `TBS_USER_ONBOARD_ORG_VENUE_ID`  | Service Manager option           | 119     |

Align any frontend constants or feature flags with these if you duplicate ids client-side.

## Example payloads

**TBS Pre-Sales**

```json
{
  "email": "alex@example.com",
  "firstName": "Alex",
  "lastName": "Kim",
  "role": "TBS_PRESALES",
  "isTbsUser": true
}
```

**TBS Service Manager (TBS Planning)**

```json
{
  "email": "sam@example.com",
  "firstName": "Sam",
  "lastName": "Lee",
  "role": "TBS_SVC_MANAGER",
  "isTbsUser": true,
  "tbsOrganizationId": 118
}
```

## Updating users (`PUT /api/users/{id}`)

- A user created as TBS (`isTbsUser: true` in the database) **must** keep a `TBS_*` role.  
- You cannot turn an Acceltancy user into a TBS user via update; create a new user with `isTbsUser: true` instead.

## Troubleshooting: `Role not found: TBS_PRESALES` (or similar)

The API loads roles from the **`roles`** table. Older databases often define `roles.name` as a **MySQL `ENUM('ADMIN','CATEGORY_MANAGER','SALES','PRESALES')`**, which **rejects** inserting new role names until the column is widened.

The backend now:

1. On startup (`TbsUserSchemaAndRolesBootstrap`) and **again before each TBS user create** (`TbsRoleProvisioning.ensureTbsRolesPresent()`), converts `roles.name` from `ENUM` to **`VARCHAR(64)`** when needed, renames legacy rows (`TBS_PRE_SALES` → `TBS_PRESALES`, etc.), then inserts the three TBS rows if missing.

If creation still fails (e.g. DB user lacks `ALTER` permission), run manually:

```sql
ALTER TABLE roles MODIFY COLUMN name VARCHAR(64) NOT NULL;

UPDATE roles SET name = 'TBS_PRESALES' WHERE name = 'TBS_PRE_SALES';
UPDATE roles SET name = 'TBS_REL_MANAGER' WHERE name = 'TBS_RELATIONSHIP_MANAGER';
UPDATE roles SET name = 'TBS_SVC_MANAGER' WHERE name = 'TBS_SERVICE_MANAGER';

INSERT IGNORE INTO roles (name, description) VALUES
('TBS_PRESALES', 'TBS onboarding: pre-sales'),
('TBS_REL_MANAGER', 'TBS onboarding: relationship manager'),
('TBS_SVC_MANAGER', 'TBS onboarding: service manager');
```

## Security / navigation notes

- TBS users receive **both** their real role authority and **`ROLE_SALES`** for compatibility with existing `@PreAuthorize(hasRole('SALES'))` endpoints.  
- Deal and pipeline visibility for TBS users is scoped to **`tbsDefaultPipelineId`** and the home organization; the UI should not assume full org-wide pipelines unless product requirements change.
