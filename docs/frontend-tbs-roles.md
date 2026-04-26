# Frontend reference: TBS and Acceltancy roles

This document summarizes **role-related** API values after the TBS onboarding work. Use it to update dropdowns, TypeScript enums, and validation.

---

## Acceltancy flow (unchanged)

Send `isTbsUser: false` or omit it. Allowed `role` values:

| `role` (string)   | Notes |
|-------------------|--------|
| `ADMIN`           | Unchanged |
| `CATEGORY_MANAGER`| Unchanged |
| `SALES`           | Unchanged |
| `PRESALES`        | Unchanged |

Existing rules apply (`managerId`, `managedCategoryId`, etc.).

---

## TBS flow

Send **`isTbsUser: true`**. The `role` field **must** be exactly one of:

| `role` (string)     | Meaning | `tbsOrganizationId` |
|---------------------|---------|---------------------|
| **`TBS_PRESALES`**  | TBS pre-sales | Omit (backend uses TBS Test, default org id **117**) |
| **`TBS_REL_MANAGER`** | TBS relationship manager | Omit (same default org **117**) |
| **`TBS_SVC_MANAGER`** | TBS service manager | **Required** — one of **64** (Revaah), **118** (TBS Planning), **119** (TBS Venue) |

### Naming note (ENUM-friendly)

These names replace older drafts (`TBS_PRE_SALES`, `TBS_RELATIONSHIP_MANAGER`, `TBS_SERVICE_MANAGER`). The backend migrates old `roles` rows on startup / before create; the **API and UI should use only the new strings** above.

---

## `POST /api/users` body (role-related)

**Acceltancy**

```json
{
  "email": "…",
  "firstName": "…",
  "lastName": "…",
  "role": "SALES",
  "isTbsUser": false,
  "managerId": 123
}
```

**TBS — pre-sales**

```json
{
  "email": "…",
  "firstName": "…",
  "lastName": "…",
  "role": "TBS_PRESALES",
  "isTbsUser": true
}
```

**TBS — service manager**

```json
{
  "email": "…",
  "firstName": "…",
  "lastName": "…",
  "role": "TBS_SVC_MANAGER",
  "isTbsUser": true,
  "tbsOrganizationId": 118
}
```

Rules:

- `isTbsUser === true` ⟺ `role` is one of `TBS_PRESALES`, `TBS_REL_MANAGER`, `TBS_SVC_MANAGER` (otherwise **400**).
- Do not send `managerId` for TBS users (ignored / not used).

---

## `GET /api/users/managers?forRole=…`

Query parameter `forRole` accepts the same role strings, including **`TBS_PRESALES`**, **`TBS_REL_MANAGER`**, **`TBS_SVC_MANAGER`**. For those TBS roles the list is **empty** — hide the manager dropdown.

---

## `UserResponse` (role-related extras)

For TBS users the API may include:

| Field | Type | Description |
|-------|------|-------------|
| `role` | string | One of the values above |
| `isTbsUser` | boolean | `true` for TBS onboarding users |
| `tbsHomeOrganizationId` | number \| null | Org tied to the TBS pipeline |
| `tbsDefaultPipelineId` | number \| null | Default pipeline created for that user |

---

## JWT / Spring Security (optional UI routing)

Authorities include `ROLE_<ENUM_NAME>` (e.g. `ROLE_TBS_PRESALES`). TBS users **also** receive **`ROLE_SALES`** so existing endpoints guarded with `hasRole('SALES')` keep working. Prefer **backend-driven** menu/page flags (e.g. page-access APIs) over hard-coding authority checks in the client when possible.

---

## Config alignment (org ids)

Optional env vars (defaults in parentheses) for the three service-manager orgs and TBS Test:

- `TBS_USER_ONBOARD_ORG_TEST_ID` (**117**)
- `TBS_USER_ONBOARD_ORG_REVAAH_ID` (**64**)
- `TBS_USER_ONBOARD_ORG_PLANNING_ID` (**118**)
- `TBS_USER_ONBOARD_ORG_VENUE_ID` (**119**)

Mirror these in the frontend if you hard-code org pickers.

---

## Related doc

Full create-user flow (pipelines, stages, troubleshooting): **`frontend-tbs-user-flow.md`** in this folder.
