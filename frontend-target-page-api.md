# Target Page API - Frontend Developer Guide

## Table of Contents
1. [Overview](#overview)
2. [Base URL & Authentication](#base-url--authentication)
3. [API Endpoints](#api-endpoints)
4. [Data Models](#data-models)
5. [Category System](#category-system)
6. [Period Types](#period-types)
7. [Time Presets](#time-presets)
8. [Frontend Implementation Guide](#frontend-implementation-guide)
9. [Examples](#examples)

---

## Overview

The Target Page is a dashboard for tracking sales targets and achievements. It allows:
- **Admins** to create, update, and delete sales targets for different categories and time periods
- **All authenticated users** to view target vs achievement metrics
- Support for multiple period types: Monthly, Quarterly, Half-Yearly, and Yearly
- Organization-based targets (targets can be linked to specific organizations)

### Key Features
- Dashboard view showing targets vs achievements by category
- Support for multiple time periods (monthly, quarterly, half-yearly, yearly)
- Deal tracking and achievement calculation
- Incentive calculations
- Filtering by category and time range
- Organization-based target assignment

---

## Base URL & Authentication

**Base URL:** `/api/targets`

**Authentication:** All endpoints require JWT authentication. Include the token in the Authorization header:
```
Authorization: Bearer <your-jwt-token>
```

**Role Requirements:**
- Dashboard and filters: Any authenticated user
- Create/Update/Delete/List: ADMIN role only

---

## API Endpoints

### 1. Get Dashboard Data
**GET** `/api/targets/dashboard`

Returns target vs achievement metrics organized by month and category, plus a list of won deals.

**Query Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `category` | string | No | Filter by category code (PHOTOGRAPHY, MAKEUP, PLANNING_AND_DECOR) |
| `timePreset` | string | No | Time preset (see [Time Presets](#time-presets)) |
| `month` | integer | No | Month (1-12) - used with timePreset or custom |
| `year` | integer | No | Year (e.g., 2025) |
| `fromMonth` | integer | No | Start month for custom range (1-12) |
| `fromYear` | integer | No | Start year for custom range |
| `toMonth` | integer | No | End month for custom range (1-12) |
| `toYear` | integer | No | End year for custom range |

**Response:** `DashboardResponse`

**Example Request:**
```javascript
GET /api/targets/dashboard?timePreset=THIS_MONTH&category=PHOTOGRAPHY
```

---

### 2. Get Filter Metadata
**GET** `/api/targets/filters`

Returns available filter options (categories, time presets, minimum year).

**Response:** `FiltersResponse`

**Example Response:**
```json
{
  "success": true,
  "message": "Target filters fetched",
  "data": {
    "categories": [
      { "code": "PHOTOGRAPHY", "label": "Photography" },
      { "code": "MAKEUP", "label": "Makeup" },
      { "code": "PLANNING_AND_DECOR", "label": "Planning & Decor" }
    ],
    "presets": [
      { "code": "THIS_MONTH", "label": "This month" },
      { "code": "PREVIOUS_MONTH", "label": "Previous month" },
      // ... more presets
    ],
    "minYear": 2025
  }
}
```

---

### 3. List Targets (Admin Only)
**GET** `/api/targets`

Lists all targets for a specific month and category. Used by admins to manage targets.

**Query Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `month` | integer | No | Month (1-12), defaults to current month |
| `year` | integer | No | Year, defaults to current year |
| `category` | string | No | Filter by category code |

**Response:** `List<TargetResponse>`

---

### 4. Create Target (Admin Only)
**POST** `/api/targets`

Creates a new sales target.

**Request Body:** `TargetUpsertRequest`

**Response:** `TargetResponse`

---

### 5. Update Target (Admin Only)
**PUT** `/api/targets/{id}`

Updates an existing target.

**Path Parameters:**
- `id` (Long): Target ID

**Request Body:** `TargetUpsertRequest`

**Response:** `TargetResponse`

---

### 6. Delete Target (Admin Only)
**DELETE** `/api/targets/{id}`

Deletes a target.

**Path Parameters:**
- `id` (Long): Target ID

**Response:** Success message

---

### 7. Get Sales Users with Organizations (Admin Only)
**GET** `/api/targets/sales-users`

Returns all SALES role users with their associated organizations.

**Response:** `List<SalesUserWithOrganizations>`

---

### 8. Get Organizations for Sales User (Admin Only)
**GET** `/api/targets/sales-users/{userId}/organizations`

Returns organizations assigned to a specific sales user, grouped by month.

**Path Parameters:**
- `userId` (Long): User ID

**Response:** `SalesUserOrganizationsResponse`

---

### 9. Get User Monthly Target Detail (Admin Only)
**GET** `/api/targets/users/{userId}/detail`

Returns month-by-month statistics for a sales user for the specified year.

**Path Parameters:**
- `userId` (Long): User ID

**Query Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `year` | integer | Yes | Calendar year (>= minYear) |

**Response:** `TargetUserMonthlyDetailResponse`

**Example Request:**  
`GET /api/targets/users/13/detail?year=2025`

**Example Response (truncated):**
```json
{
  "success": true,
  "message": "User monthly detail fetched",
  "data": {
    "userId": 13,
    "userName": "Shubham Lohra",
    "year": 2025,
    "monthlyData": [
      {
        "month": 1,
        "year": 2025,
        "target": 50000.00,
        "achieved": 42000.00,
        "achievementPercent": 84.00,
        "totalDeals": 3,
        "incentive": 4200.00,
        "diversionDeals": 1,
        "instaDeals": 1,
        "referenceDeals": 0,
        "plannerDeals": 1
      }
    ]
  }
}
```

---

### 10. Get Category Monthly Breakdown
**GET** `/api/targets/category-breakdown`

Returns month-wise totals plus per-user rows for a single category. Use this for the **Yearly / Half-Yearly / Quarterly** category view tabs.

**Query Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `category` | string | Yes | Category code (PHOTOGRAPHY, MAKEUP, PLANNING_AND_DECOR) |
| `timePreset` | string | Yes | Must be `THIS_YEAR`, `HALF_YEAR`, `THIS_QUARTER`, or `CUSTOM_RANGE` |
| `month` | integer | No | Anchor month for quarter calculations (defaults to current month) |
| `year` | integer | No | Anchor year (defaults to current year) |
| `fromMonth` / `fromYear` | integer | No | Required when `timePreset=CUSTOM_RANGE` |
| `toMonth` / `toYear` | integer | No | Required when `timePreset=CUSTOM_RANGE` |

> **Note:** When using `CUSTOM_RANGE`, pass inclusive `from`/`to` months (same as the dashboard) so the backend can build the period list.

**Response:** `CategoryMonthlyBreakdownResponse`

**Example Request:**  
`GET /api/targets/category-breakdown?category=PHOTOGRAPHY&timePreset=THIS_YEAR&year=2025`

**Example Response (trimmed):**
```json
{
  "success": true,
  "message": "Category breakdown fetched",
  "data": {
    "filters": {
      "timePreset": "THIS_YEAR",
      "year": 2025,
      "category": "PHOTOGRAPHY",
      "editableForCurrentUser": false
    },
    "category": "PHOTOGRAPHY",
    "categoryLabel": "Photography",
    "months": [
      {
        "month": 11,
        "year": 2025,
        "totalTarget": 500000,
        "achieved": 1200000,
        "achievementPercent": 240,
        "totalDeals": 5,
        "incentivePercent": 15,
        "incentiveAmount": 180000,
        "users": [
          {
            "userId": 13,
            "userName": "Shubham Kumar",
            "totalTarget": 500000,
            "achieved": 1200000,
            "achievementPercent": 240,
            "totalDeals": 5,
            "incentivePercent": 15,
            "incentiveAmount": 180000
          }
        ]
      }
    ],
    "totals": {
      "totalTarget": 500000,
      "achieved": 1200000,
      "achievementPercent": 240,
      "totalDeals": 5,
      "incentivePercent": 15,
      "incentiveAmount": 180000
    }
  }
}
```

---

## Data Models

### TargetResponse
Response model for target data.

```typescript
interface TargetResponse {
  id: number;
  userId: number;
  userName: string;
  category: string; // PHOTOGRAPHY, MAKEUP, PLANNING_AND_DECOR
  month?: number; // 1-12, present for MONTHLY, QUARTERLY, HALF_YEARLY
  year: number;
  quarter?: number; // 1-4, present only for QUARTERLY
  halfYear?: number; // 1 or 2, present only for HALF_YEARLY
  organizationIds: number[];
  organizations: OrganizationSummary[];
  targetAmount: number;
  editable: boolean;
}
```

**Important:** The `periodType` is NOT included in the response. You must infer it from the fields:
- If `quarter` is set → **QUARTERLY**
- If `halfYear` is set → **HALF_YEARLY**
- If `month` is set (and `quarter`/`halfYear` are null) → **MONTHLY**
- If only `year` is set → **YEARLY**

### TargetUpsertRequest
Request model for creating/updating targets.

```typescript
interface TargetUpsertRequest {
  userId: number;
  category: string; // PHOTOGRAPHY, MAKEUP, PLANNING_AND_DECOR
  periodType: string; // MONTHLY, QUARTERLY, HALF_YEARLY, YEARLY
  month?: number; // Required for MONTHLY, QUARTERLY, HALF_YEARLY
  year: number; // Required
  quarter?: number; // Required for QUARTERLY (1-4)
  halfYear?: number; // Required for HALF_YEARLY (1 or 2)
  organizationIds?: number[]; // Optional, for organization-specific targets
  targetAmount: number;
}
```

### DashboardResponse
Main dashboard data structure.

```typescript
interface DashboardResponse {
  filters: AppliedFilters;
  months: MonthBlock[];
  deals: DealSummary[];
}

interface AppliedFilters {
  timePreset?: string;
  month?: number;
  year?: number;
  fromMonth?: number;
  fromYear?: number;
  toMonth?: number;
  toYear?: number;
  category?: string;
  editableForCurrentUser: boolean;
}

interface MonthBlock {
  month: number; // 1-12
  year: number;
  editable: boolean;
  categories: CategoryTable[];
}

interface CategoryTable {
  category: string; // Category code
  categoryLabel: string; // Human-readable label
  rows: TargetRow[];
}

interface TargetRow {
  userId: number;
  userName: string;
  totalTarget: number;
  achieved: number;
  achievementPercent: number;
  totalDeals: number;
  incentivePercent: number;
  incentiveAmount: number;
}

interface DealSummary {
  dealId: number;
  dealName: string;
  instagramId?: string;
  dealValue: number;
  commissionAmount: number;
  dealSource?: string;
  personSource?: string;
  phoneNumber?: string;
  venue?: string;
  eventDate?: string;
  organization?: string;
  category: string;
  userId: number;
  userName: string;
}

interface CategoryMonthlyBreakdownResponse {
  filters: AppliedFilters;
  category: string;
  categoryLabel: string;
  months: CategoryMonthlyRow[];
  totals: CategoryMonthlyTotals;
}

interface CategoryMonthlyRow {
  month: number;
  year: number;
  totalTarget: number;
  achieved: number;
  achievementPercent: number;
  totalDeals: number;
  incentivePercent: number;
  incentiveAmount: number;
  users: TargetRow[];
}

interface CategoryMonthlyTotals {
  totalTarget: number;
  achieved: number;
  achievementPercent: number;
  totalDeals: number;
  incentivePercent: number;
  incentiveAmount: number;
}
```

### OrganizationSummary
```typescript
interface OrganizationSummary {
  id: number;
  name: string;
  category: string;
}
```

### TargetUserMonthlyDetailResponse
```typescript
interface TargetUserMonthlyDetailResponse {
  userId: number;
  userName: string;
  year: number;
  /**
   * @deprecated - use availableCategories instead
   */
  categories?: string[];
  /**
   * @deprecated - use availableOrganizationIds instead
   */
  organizationIds?: number[];
  /**
   * @deprecated - use availableOrganizations instead
   */
  organizations?: OrganizationSummary[];
  availableCategories: string[];
  availableOrganizationIds: number[];
  availableOrganizations: OrganizationSummary[];
  monthlyData: UserMonthlyBreakdown[];
}

interface UserMonthlyBreakdown {
  month: number;
  year: number;
  target?: number | null;
  achieved: number;
  achievementPercent: number;
  totalDeals: number;
  incentive: number;
  diversionDeals: number;
  instaDeals: number;
  referenceDeals: number;
  plannerDeals: number;
}
```

---

## Category System

The system uses three main categories:

| Code | Label | Description |
|------|-------|-------------|
| `PHOTOGRAPHY` | Photography | Photography services |
| `MAKEUP` | Makeup | Makeup services |
| `PLANNING_AND_DECOR` | Planning & Decor | Planning and decoration services |

**Important:** The API returns category codes (e.g., "PHOTOGRAPHY"). You can map these to labels using the `/api/targets/filters` endpoint or use the `categoryLabel` field in `CategoryTable` for dashboard display.

**Category Mapping Function:**
```javascript
function getCategoryLabel(code) {
  const categoryMap = {
    'PHOTOGRAPHY': 'Photography',
    'MAKEUP': 'Makeup',
    'PLANNING_AND_DECOR': 'Planning & Decor'
  };
  return categoryMap[code] || code;
}
```

---

## Period Types

Targets can be set for different time periods:

| Period Type | Description | Required Fields |
|-------------|-------------|-----------------|
| `MONTHLY` | Single month | `year`, `month` |
| `QUARTERLY` | 3 months (Q1-Q4) | `year`, `quarter` (1-4), `month` (start month) |
| `HALF_YEARLY` | 6 months (H1 or H2) | `year`, `halfYear` (1 or 2), `month` (start month) |
| `YEARLY` | Full year | `year` |

### Inferring Period Type from Response

Since `periodType` is not included in `TargetResponse`, infer it like this:

```javascript
function inferPeriodType(target) {
  if (target.quarter !== null && target.quarter !== undefined) {
    return 'QUARTERLY';
  }
  if (target.halfYear !== null && target.halfYear !== undefined) {
    return 'HALF_YEARLY';
  }
  if (target.month !== null && target.month !== undefined) {
    return 'MONTHLY';
  }
  return 'YEARLY';
}
```

### Period Type Rules

**For Creating/Updating:**
- **MONTHLY**: Provide `year`, `month` (1-12), `periodType: "MONTHLY"`
- **QUARTERLY**: Provide `year`, `quarter` (1-4), `month` (will be calculated as quarter start month), `periodType: "QUARTERLY"`
  - Q1 = months 1-3, Q2 = months 4-6, Q3 = months 7-9, Q4 = months 10-12
- **HALF_YEARLY**: Provide `year`, `halfYear` (1 or 2), `month` (will be calculated), `periodType: "HALF_YEARLY"`
  - H1 = months 1-6, H2 = months 7-12
- **YEARLY**: Provide `year`, `periodType: "YEARLY"`

---

## Time Presets

Time presets simplify date selection:

| Preset Code | Description |
|------------|-------------|
| `THIS_MONTH` | Current month |
| `PREVIOUS_MONTH` | Previous month |
| `NEXT_MONTH` | Next month |
| `THIS_QUARTER` | Current quarter |
| `HALF_YEAR` | Current half year |
| `THIS_YEAR` | Current year |
| `CUSTOM_MONTH` | User-selected month (requires `month` and `year`) |
| `CUSTOM_RANGE` | User-selected range (requires `fromMonth`, `fromYear`, `toMonth`, `toYear`) |

**Usage:**
```javascript
// This month
GET /api/targets/dashboard?timePreset=THIS_MONTH

// Custom month
GET /api/targets/dashboard?timePreset=CUSTOM_MONTH&month=3&year=2025

// Custom range
GET /api/targets/dashboard?timePreset=CUSTOM_RANGE&fromMonth=1&fromYear=2025&toMonth=3&toYear=2025
```

---

## Frontend Implementation Guide

### Step 1: Initialize Filters

On page load, fetch filter metadata:

```javascript
async function loadFilters() {
  const response = await fetch('/api/targets/filters', {
    headers: {
      'Authorization': `Bearer ${token}`
    }
  });
  const data = await response.json();
  return data.data; // { categories, presets, minYear }
}
```

### Step 2: Load Dashboard Data

```javascript
async function loadDashboard(filters) {
  const params = new URLSearchParams();
  
  if (filters.category) params.append('category', filters.category);
  if (filters.timePreset) params.append('timePreset', filters.timePreset);
  if (filters.month) params.append('month', filters.month);
  if (filters.year) params.append('year', filters.year);
  if (filters.fromMonth) params.append('fromMonth', filters.fromMonth);
  if (filters.fromYear) params.append('fromYear', filters.fromYear);
  if (filters.toMonth) params.append('toMonth', filters.toMonth);
  if (filters.toYear) params.append('toYear', filters.toYear);
  
  const response = await fetch(`/api/targets/dashboard?${params}`, {
    headers: {
      'Authorization': `Bearer ${token}`
    }
  });
  return await response.json();
}
```

### Step 3: Display Dashboard

The dashboard response contains:
- `months`: Array of month blocks, each containing category tables
- `deals`: Array of won deals in the selected period
- `filters`: Applied filter information

**Example Rendering:**
```javascript
function renderDashboard(dashboardData) {
  const { months, deals, filters } = dashboardData.data;
  
  // Render month blocks
  months.forEach(monthBlock => {
    console.log(`Month: ${monthBlock.month}/${monthBlock.year}`);
    console.log(`Editable: ${monthBlock.editable}`);
    
    monthBlock.categories.forEach(categoryTable => {
      console.log(`Category: ${categoryTable.categoryLabel}`);
      
      categoryTable.rows.forEach(row => {
        console.log(`User: ${row.userName}`);
        console.log(`Target: ${row.totalTarget}, Achieved: ${row.achieved}`);
        console.log(`Achievement: ${row.achievementPercent}%`);
      });
    });
  });
  
  // Render deals
  deals.forEach(deal => {
    console.log(`Deal: ${deal.dealName}, Value: ${deal.dealValue}`);
  });
}
```

#### Step 3b: Toggle Category View Modes

When a **category** and a **longer time preset** (`THIS_YEAR`, `HALF_YEAR`, `THIS_QUARTER`, or `CUSTOM_RANGE`) are selected, expose three tabs:
1. **Users (default)** – existing dashboard layout (no extra API calls).
2. **Monthly Totals** – call `/api/targets/category-breakdown` and render a single table (`months` array → 12/6/3 rows). Use `totals` for the footer row.
3. **Monthly Grid** – reuse the same category breakdown data but display two month-cards per row (six rows for 12 months, three rows for 6 months, etc.). Each card shows the month header plus the per-user rows from `CategoryMonthlyRow.users`.

Example loader:
```javascript
async function loadCategoryBreakdown(filters) {
  const params = new URLSearchParams({
    category: filters.category,
    timePreset: filters.timePreset,
    year: filters.year,
    month: filters.month
  });
  const response = await fetch(`/api/targets/category-breakdown?${params.toString()}`, {
    headers: { 'Authorization': `Bearer ${token}` }
  });
  return (await response.json()).data;
}
```

UI tips:
- Disable the extra tabs until both `category` and a supported preset are set.
- Preserve the same columns as the user detail table (`Target`, `Achieved`, `%`, `Total Deals`, `Incentive`).
- For the grid, chunk the `months` array with a helper such as `chunk(months, 2)` so you can render two cards per row for yearly mode.

### Step 4: Create/Update Target (Admin)

```javascript
async function createTarget(targetData) {
  const response = await fetch('/api/targets', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`
    },
    body: JSON.stringify({
      userId: targetData.userId,
      category: targetData.category, // PHOTOGRAPHY, MAKEUP, PLANNING_AND_DECOR
      periodType: targetData.periodType, // MONTHLY, QUARTERLY, HALF_YEARLY, YEARLY
      year: targetData.year,
      month: targetData.month, // Required for MONTHLY, QUARTERLY, HALF_YEARLY
      quarter: targetData.quarter, // Required for QUARTERLY (1-4)
      halfYear: targetData.halfYear, // Required for HALF_YEARLY (1 or 2)
      organizationIds: targetData.organizationIds || [], // Optional
      targetAmount: targetData.targetAmount
    })
  });
  return await response.json();
}
```

**Validation Rules:**
- For `MONTHLY`: `month` is required
- For `QUARTERLY`: `quarter` is required
- For `HALF_YEARLY`: `halfYear` is required
- For `YEARLY`: Only `year` is needed

### Step 5: Handle Period Type Inference

When displaying targets, infer the period type:

```javascript
function getPeriodTypeLabel(target) {
  const periodType = inferPeriodType(target);
  
  switch (periodType) {
    case 'MONTHLY':
      return `${getMonthName(target.month)} ${target.year}`;
    case 'QUARTERLY':
      return `Q${target.quarter} ${target.year}`;
    case 'HALF_YEARLY':
      return `H${target.halfYear} ${target.year}`;
    case 'YEARLY':
      return `${target.year}`;
    default:
      return 'Unknown';
  }
}
```

### Step 6: Load User Monthly Detail (Admin)

Admins can open the user detail page by fetching the new endpoint:

```javascript
async function loadUserMonthlyDetail(userId, year) {
  const response = await fetch(`/api/targets/users/${userId}/detail?year=${year}`, {
    headers: { 'Authorization': `Bearer ${token}` }
  });
  const result = await response.json();
  return result.data; // { userId, userName, year, monthlyData: [...] }
}
```

Use the `monthlyData` array to render the 12-month table and the totals row.

### Step 7: Error Handling

```javascript
async function handleApiCall(apiFunction) {
  try {
    const response = await apiFunction();
    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.message || 'API request failed');
    }
    return await response.json();
  } catch (error) {
    console.error('API Error:', error);
    // Show user-friendly error message
    alert(`Error: ${error.message}`);
    throw error;
  }
}
```

---

## Examples

### Example 1: Get This Month's Dashboard

```javascript
const response = await fetch('/api/targets/dashboard?timePreset=THIS_MONTH', {
  headers: { 'Authorization': `Bearer ${token}` }
});
const data = await response.json();
```

### Example 2: Create Monthly Target

```javascript
const target = {
  userId: 123,
  category: 'PHOTOGRAPHY',
  periodType: 'MONTHLY',
  year: 2025,
  month: 3,
  targetAmount: 50000.00,
  organizationIds: [1, 2, 3] // Optional
};

const response = await fetch('/api/targets', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
    'Authorization': `Bearer ${token}`
  },
  body: JSON.stringify(target)
});
```

### Example 3: Create Quarterly Target

```javascript
const target = {
  userId: 123,
  category: 'MAKEUP',
  periodType: 'QUARTERLY',
  year: 2025,
  quarter: 1, // Q1 (Jan-Mar)
  month: 1, // Start month (will be calculated, but include it)
  targetAmount: 150000.00
};
```

### Example 4: Update Target

```javascript
const targetId = 456;
const updatedTarget = {
  userId: 123,
  category: 'PHOTOGRAPHY',
  periodType: 'MONTHLY',
  year: 2025,
  month: 3,
  targetAmount: 60000.00 // Updated amount
};

const response = await fetch(`/api/targets/${targetId}`, {
  method: 'PUT',
  headers: {
    'Content-Type': 'application/json',
    'Authorization': `Bearer ${token}`
  },
  body: JSON.stringify(updatedTarget)
});
```

### Example 5: Get Sales Users for Target Creation

```javascript
// Get all sales users with their organizations
const response = await fetch('/api/targets/sales-users', {
  headers: { 'Authorization': `Bearer ${token}` }
});
const users = await response.json();

// For a specific user, get their organizations by month
const userId = 123;
const orgResponse = await fetch(`/api/targets/sales-users/${userId}/organizations`, {
  headers: { 'Authorization': `Bearer ${token}` }
});
const userOrgs = await orgResponse.json();
```

---

### Example 6: Fetch Monthly Detail for a User

```javascript
const userId = 13;
const year = 2025;

const detailResponse = await fetch(`/api/targets/users/${userId}/detail?year=${year}`, {
  headers: { 'Authorization': `Bearer ${token}` }
});
const detail = await detailResponse.json();

console.log(detail.data.monthlyData); // array of 12 rows
```

### Example 7: Load Category Monthly Breakdown

```javascript
const params = new URLSearchParams({
  category: 'PHOTOGRAPHY',
  timePreset: 'THIS_YEAR',
  year: 2025
});

const response = await fetch(`/api/targets/category-breakdown?${params.toString()}`, {
  headers: { 'Authorization': `Bearer ${token}` }
});

const breakdown = await response.json();
console.log(breakdown.data.months); // array of month rows; each contains per-user rows
console.log(breakdown.data.totals); // aggregate totals for footer
```

### Example 8: Fetch Target For Editing

```javascript
const targetId = 456;

const response = await fetch(`/api/targets/${targetId}`, {
  headers: {
    'Authorization': `Bearer ${token}`
  }
});

const result = await response.json();
const target = result.data; // Pass this data to pre-fill the Set Target form
```

---

## Important Notes

1. **Period Type Inference**: The `periodType` field is NOT in the response. Always infer it from the presence of `quarter`, `halfYear`, or `month` fields.

2. **Category Labels**: Use the `categoryLabel` from `CategoryTable` in dashboard, or map category codes using the filters endpoint.

3. **Editable Field**: The `editable` field in `TargetResponse` and `MonthBlock` indicates whether the current user can edit targets for that period. This is based on the user's role and the date (past periods may not be editable).

4. **Organization Targets**: Targets can be linked to specific organizations. When creating a target, you can optionally provide `organizationIds`. If provided, the target applies only to deals from those organizations.

5. **Achievement Calculation**: Achievements are calculated from won deals that were updated in the target period. The system matches deals to targets based on:
   - Deal category
   - Deal owner (user)
   - Deal update date

6. **Incentive Calculation**: Incentive percentages and amounts are calculated based on achievement percentages (business logic in backend).

7. **Date Handling**: All dates are in the server's timezone. The `minYear` from filters endpoint indicates the earliest year for which targets can be created.

---

## Support

For questions or issues, contact the backend team or refer to the Swagger documentation at:
`http://localhost:8080/swagger-ui.html` (when running locally)

---

**Last Updated:** November 2025