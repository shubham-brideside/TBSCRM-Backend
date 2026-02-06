## Admin Dashboard - Won Deals by Sales User (Frontend Guide)

### Overview

This document describes the backend endpoint that provides **won deals aggregated per SALES user** for the Admin dashboard.  
Deals are attributed using the chain: **Deal → Pipeline → Organization → Owner (SALES role)**.

Use this to build cards/tables like **“Deals Won per Sales User”** without changing existing deal logic.

---

### Base URL & Authentication

- **Base URL:** `/api/admin`
- **Authentication:** JWT required, include header:

```http
Authorization: Bearer <your-jwt-token>
```

- **Roles:**
  - Endpoint requires **ADMIN** role.

---

### Endpoint: Get Won Deals Grouped by Sales User (All Time)

- **Method:** `GET`
- **URL:** `/api/admin/dashboard/won-deals-by-sales-user`
- **Auth:** Admin-only, authenticated
- **Query Params:** _none_

#### Attribution Rules

- Only deals with:
  - `status = WON`
  - `isDeleted = false`
- A deal is counted for a SALES user **only if**:
  - `deal.pipeline` is not null
  - `deal.pipeline.organization` is not null
  - `deal.pipeline.organization.owner` exists and has role `SALES`
- All such deals for a given SALES user are aggregated.

#### Response Shape (All Time)

Standard wrapped `ApiResponse`:

```json
{
  "success": true,
  "message": "Won deals grouped by sales user fetched",
  "data": {
    "users": [
      {
        "userId": 123,
        "userName": "Alice Smith",
        "email": "alice@example.com",
        "totalDeals": 7,
        "totalDealValue": 250000.00
      }
    ]
  }
}
```

- **`users`**: array of aggregated rows
  - **`userId`**: ID of the SALES user
  - **`userName`**: `"firstName lastName"` (trimmed)
  - **`email`**: user’s login email
  - **`totalDeals`**: number of WON deals attributed to this user
  - **`totalDealValue`**: sum of `deal.value` across those deals (BigDecimal)

---

### Endpoint: Get Monthly Won Deals per Sales User (By Year)

- **Method:** `GET`
- **URL:** `/api/admin/dashboard/won-deals-by-sales-user/monthly`
- **Auth:** Admin-only, authenticated

**Query Parameters:**

| Name  | Type    | Required | Description                      |
|-------|---------|----------|----------------------------------|
| year  | integer | Yes      | Calendar year, e.g. `2025`      |

#### Response Shape (Monthly, Per User)

```json
{
  "success": true,
  "message": "Monthly won deals grouped by sales user fetched",
  "data": {
    "year": 2025,
    "users": [
      {
        "userId": 123,
        "userName": "Alice Smith",
        "email": "alice@example.com",
        "months": [
          { "month": 1, "totalDeals": 2, "totalDealValue": 50000.00 },
          { "month": 2, "totalDeals": 0, "totalDealValue": 0.00 },
          { "month": 3, "totalDeals": 1, "totalDealValue": 20000.00 }
          // ... up to month 12
        ]
      }
    ]
  }
}
```

- **`year`**: the year you requested.
- **`users`**: array of per-user rows.
  - **`userId`**, **`userName`**, **`email`**: same as all-time endpoint.
  - **`months`**: always includes entries for months `1` through `12`:
    - **`month`**: month number (1–12)
    - **`totalDeals`**: number of WON deals for that user in that month
    - **`totalDealValue`**: sum of `deal.value` for that user and month.


---

### Frontend Usage Example

```javascript
async function loadWonDealsBySalesUser() {
  const response = await authenticatedFetch(
    `${API_BASE_URL}/api/admin/dashboard/won-deals-by-sales-user`
  );

  if (!response.ok) {
    console.error('Failed to load won deals per user');
    return;
  }

  const json = await response.json();
  if (!json.success || !json.data) return;

  const rows = json.data.users || [];
  // TODO: bind `rows` to your Admin dashboard table/cards
}
```

Recommended UI:

- Show a table with columns:
  - **Sales User**, **Email**, **Total Won Deals**, **Total Value**
- Optionally sort by **Total Value** or **Total Won Deals** descending for quick insight.


