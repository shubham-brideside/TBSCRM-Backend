# Category Manager Dashboard API - Frontend Developer Guide

## Table of Contents
1. [Overview](#overview)
2. [Base URL & Authentication](#base-url--authentication)
3. [Data Scoping](#data-scoping)
4. [API Endpoints](#api-endpoints)
5. [Data Models](#data-models)
6. [Frontend Implementation Guide](#frontend-implementation-guide)
7. [Examples](#examples)

---

## Overview

The Category Manager Dashboard provides **admin-style metrics scoped to the Category Manager's hierarchy**. This dashboard shows category-level analytics (by organization category, user, and pipeline) for:

- The Category Manager themselves
- All Sales managers reporting directly to them
- All Presales users under those Sales managers

### Key Features
- **Summary stats**: Team counts (Sales/Presales), deal totals (won/lost/in progress), revenue (all-time and YTD)
- **Revenue breakdown**: By organization category (Photography, Makeup, Planning and Decor), by user, and by pipeline
- **Deal analytics**: Won/lost deals grouped by user, monthly deal status summary
- **Lost deal analysis**: Lost deal reasons with counts and percentages
- **Category-level filtering**: All metrics are automatically scoped to the Category Manager's team

---

## Base URL & Authentication

**Base URL:** `/api/category-manager/dashboard`

**Authentication:** All endpoints require JWT authentication. Include the token in the Authorization header:
```
Authorization: Bearer <your-jwt-token>
```

**Role Requirements:** `CATEGORY_MANAGER` role only. All endpoints use `@PreAuthorize("hasRole('CATEGORY_MANAGER')")`.

**Standard Response Envelope:**
All endpoints return the standard envelope:
```json
{
  "success": true|false,
  "message": "Human readable message",
  "data": { ... } // optional
}
```

---

## Data Scoping

**Important:** All data returned by these endpoints is **automatically filtered** to only include:

1. **Deals** where the organization owner is:
   - The Category Manager themselves
   - Any Sales user who reports directly to this Category Manager (`user.manager_id` = Category Manager's ID)
   - Any Presales user under those Sales managers

2. **Pipelines** linked to teams where:
   - The team manager is a Sales user reporting to this Category Manager

3. **Users** in the hierarchy:
   - Category Manager + their direct Sales reports + Presales under those Sales

This means the frontend does **not** need to filter data client-side - the backend handles all scoping automatically.

---

## API Endpoints

### 1. Get Dashboard Summary

**GET** `/api/category-manager/dashboard/summary`

Returns high-level summary statistics for the Category Manager's team.

**Response:** `SummaryResponse`

**Example Request:**
```javascript
GET /api/category-manager/dashboard/summary
Authorization: Bearer <token>
```

**Example Response (200 OK):**
```json
{
  "success": true,
  "message": "Dashboard summary fetched",
  "data": {
    "salesManagersCount": 5,
    "presalesCount": 12,
    "totalTeamSize": 17,
    "totalWonDeals": 142,
    "totalLostDeals": 28,
    "totalInProgressDeals": 45,
    "totalWonValue": 1250000.50,
    "totalWonValueYtd": 450000.00
  }
}
```

**Field Descriptions:**
| Field | Type | Description |
|-------|------|-------------|
| `salesManagersCount` | Long | Number of Sales managers (direct reports) under this Category Manager |
| `presalesCount` | Long | Number of Presales users under this Category Manager (direct + under Sales) |
| `totalTeamSize` | Long | Total team size (Sales + Presales) |
| `totalWonDeals` | Long | Total WON deals (all time) in scope |
| `totalLostDeals` | Long | Total LOST deals (all time) in scope |
| `totalInProgressDeals` | Long | Total IN_PROGRESS deals in scope |
| `totalWonValue` | BigDecimal | Total value of WON deals (all time) in scope |
| `totalWonValueYtd` | BigDecimal | Total value of WON deals in current year (YTD) in scope |

---

### 2. Get Revenue Summary

**GET** `/api/category-manager/dashboard/revenue`

Returns revenue breakdown by organization category, user, and pipeline for a given date range.

**Query Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `dateFrom` | Date (ISO 8601) | Yes | Start date (inclusive), e.g., `2025-01-01` |
| `dateTo` | Date (ISO 8601) | Yes | End date (inclusive), e.g., `2025-12-31` |

**Response:** `RevenueSummaryResponse`

**Example Request:**
```javascript
GET /api/category-manager/dashboard/revenue?dateFrom=2025-01-01&dateTo=2025-12-31
Authorization: Bearer <token>
```

**Example Response (200 OK):**
```json
{
  "success": true,
  "message": "Revenue summary fetched",
  "data": {
    "dateFrom": "2025-01-01",
    "dateTo": "2025-12-31",
    "totalDeals": 89,
    "totalDealValue": 750000.00,
    "categories": [
      {
        "category": "Photography",
        "totalDeals": 45,
        "totalDealValue": 400000.00
      },
      {
        "category": "Makeup",
        "totalDeals": 30,
        "totalDealValue": 250000.00
      },
      {
        "category": "Planning and Decor",
        "totalDeals": 14,
        "totalDealValue": 100000.00
      }
    ],
    "users": [
      {
        "userId": 5,
        "userName": "Jane Doe",
        "email": "jane.doe@brideside.com",
        "totalDeals": 35,
        "totalDealValue": 300000.00
      },
      {
        "userId": 8,
        "userName": "John Smith",
        "email": "john.smith@brideside.com",
        "totalDeals": 28,
        "totalDealValue": 250000.00
      }
    ],
    "pipelines": [
      {
        "pipelineId": 12,
        "pipelineName": "Q1 Photography Pipeline",
        "totalDeals": 20,
        "totalDealValue": 180000.00
      },
      {
        "pipelineId": 15,
        "pipelineName": "Q2 Makeup Pipeline",
        "totalDeals": 15,
        "totalDealValue": 120000.00
      }
    ]
  }
}
```

**Field Descriptions:**
| Field | Type | Description |
|-------|------|-------------|
| `dateFrom` | LocalDate | Start date of the range |
| `dateTo` | LocalDate | End date of the range |
| `totalDeals` | Long | Total number of WON deals in the date range |
| `totalDealValue` | BigDecimal | Total value of WON deals in the date range |
| `categories` | Array | Revenue broken down by organization category |
| `categories[].category` | String | Organization category name (e.g., "Photography", "Makeup", "Planning and Decor") |
| `categories[].totalDeals` | Long | Number of deals for this category |
| `categories[].totalDealValue` | BigDecimal | Total value for this category |
| `users` | Array | Revenue broken down by user (SALES or CATEGORY_MANAGER) |
| `users[].userId` | Long | User ID |
| `users[].userName` | String | User's full name |
| `users[].email` | String | User's email |
| `users[].totalDeals` | Long | Number of deals attributed to this user |
| `users[].totalDealValue` | BigDecimal | Total value attributed to this user |
| `pipelines` | Array | Revenue broken down by pipeline |
| `pipelines[].pipelineId` | Long | Pipeline ID |
| `pipelines[].pipelineName` | String | Pipeline name |
| `pipelines[].totalDeals` | Long | Number of deals in this pipeline |
| `pipelines[].totalDealValue` | BigDecimal | Total value for this pipeline |

**Note:** Only WON, non-deleted deals are included. Deal attribution uses `pipeline → organization → owner` chain.

---

### 3. Get Won Deals by User

**GET** `/api/category-manager/dashboard/won-deals-by-sales-user`

Returns WON deals grouped by user (SALES or Category Manager) - all time totals.

**Response:** `WonDealsBySalesUserResponse`

**Example Request:**
```javascript
GET /api/category-manager/dashboard/won-deals-by-sales-user
Authorization: Bearer <token>
```

**Example Response (200 OK):**
```json
{
  "success": true,
  "message": "Won deals by user fetched",
  "data": {
    "users": [
      {
        "userId": 5,
        "userName": "Jane Doe",
        "email": "jane.doe@brideside.com",
        "totalDeals": 45,
        "totalDealValue": 450000.00
      },
      {
        "userId": 8,
        "userName": "John Smith",
        "email": "john.smith@brideside.com",
        "totalDeals": 38,
        "totalDealValue": 380000.00
      }
    ]
  }
}
```

**Field Descriptions:**
| Field | Type | Description |
|-------|------|-------------|
| `users` | Array | List of users with their won deal totals |
| `users[].userId` | Long | User ID |
| `users[].userName` | String | User's full name |
| `users[].email` | String | User's email |
| `users[].totalDeals` | Long | Total number of WON deals (all time) |
| `users[].totalDealValue` | BigDecimal | Total value of WON deals (all time) |

---

### 3b. Get Monthly Won Deals by Sales User (with Commission)

**GET** `/api/category-manager/dashboard/won-deals-by-sales-user/monthly`

Returns **monthly WON deals per SALES user**, including **commission amount**, scoped to the Category Manager’s hierarchy. Same shape as the admin dashboard’s monthly-won-by-sales-user endpoint.

**Query Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `year` | Integer | Yes | Year (e.g., `2026`) |
| `category` | String | No | Filter by organization category (e.g., `"Photography"`) |
| `dateFrom` | Date (YYYY-MM-DD) | No | Start date filter within the given year |
| `dateTo` | Date (YYYY-MM-DD) | No | End date filter within the given year |

**Response:** `MonthlyWonDealsBySalesUserResponse`

**Example Request:**
```http
GET /api/category-manager/dashboard/won-deals-by-sales-user/monthly?year=2026&category=Photography&dateFrom=2026-01-01&dateTo=2026-12-31
Authorization: Bearer <token>
```

**Example Response (200 OK):**
```json
{
  "success": true,
  "message": "Monthly won deals grouped by sales user fetched",
  "data": {
    "year": 2026,
    "category": "Photography",
    "users": [
      {
        "userId": 5,
        "userName": "Jane Doe",
        "email": "jane.doe@brideside.com",
        "months": [
          {
            "month": 1,
            "totalDeals": 3,
            "totalDealValue": 30000.00,
            "totalDealCommission": 4500.00
          },
          {
            "month": 2,
            "totalDeals": 2,
            "totalDealValue": 20000.00,
            "totalDealCommission": 3000.00
          }
          // ... months 3-12
        ]
      }
    ]
  }
}
```

**Field Descriptions:**
| Field | Type | Description |
|-------|------|-------------|
| `year` | Integer | Year for which data is returned |
| `category` | String | Organization category filter applied (null if none) |
| `users` | Array | One entry per SALES user in the Category Manager’s hierarchy |
| `users[].userId` | Long | SALES user ID |
| `users[].userName` | String | SALES user full name |
| `users[].email` | String | SALES user email |
| `users[].months` | Array | Exactly 12 objects (month 1–12) for this user |
| `users[].months[].month` | Integer | Month (1–12) |
| `users[].months[].totalDeals` | Long | WON deals count for this user in this month |
| `users[].months[].totalDealValue` | BigDecimal | Total WON deal value for this user in this month |
| `users[].months[].totalDealCommission` | BigDecimal | **Total commission amount** for this user’s WON deals in this month |

---

### 4. Get Lost Deals by User

**GET** `/api/category-manager/dashboard/lost-deals-by-sales-user`

Returns LOST deals grouped by user (SALES or Category Manager) - all time totals.

**Response:** `LostDealsBySalesUserResponse`

**Example Request:**
```javascript
GET /api/category-manager/dashboard/lost-deals-by-sales-user
Authorization: Bearer <token>
```

**Example Response (200 OK):**
```json
{
  "success": true,
  "message": "Lost deals by user fetched",
  "data": {
    "users": [
      {
        "userId": 5,
        "userName": "Jane Doe",
        "email": "jane.doe@brideside.com",
        "totalDeals": 8,
        "totalDealValue": 80000.00
      },
      {
        "userId": 8,
        "userName": "John Smith",
        "email": "john.smith@brideside.com",
        "totalDeals": 5,
        "totalDealValue": 50000.00
      }
    ]
  }
}
```

**Field Descriptions:** Same structure as Won Deals by User, but for LOST deals.

---

### 5. Get Monthly Deal Status Summary

**GET** `/api/category-manager/dashboard/deals-status-monthly`

Returns monthly breakdown of deal statuses (WON/LOST/IN_PROGRESS) for a specific year, with optional breakdowns by category, user, and pipeline.

**Query Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `year` | Integer | Yes | Year (e.g., `2025`) |

**Response:** `DealStatusMonthlySummaryResponse`

**Example Request:**
```javascript
GET /api/category-manager/dashboard/deals-status-monthly?year=2025
Authorization: Bearer <token>
```

**Example Response (200 OK):**
```json
{
  "success": true,
  "message": "Deal status monthly summary fetched",
  "data": {
    "year": 2025,
    "months": [
      {
        "month": 1,
        "wonCount": 12,
        "wonValue": 120000.00,
        "lostCount": 2,
        "lostValue": 20000.00,
        "inProgressCount": 8,
        "inProgressValue": 80000.00,
        "categories": [
          {
            "category": "Photography",
            "wonCount": 7,
            "wonValue": 70000.00,
            "lostCount": 1,
            "lostValue": 10000.00,
            "inProgressCount": 4,
            "inProgressValue": 40000.00
          }
        ],
        "users": [
          {
            "userId": 5,
            "userName": "Jane Doe",
            "email": "jane.doe@brideside.com",
            "wonCount": 6,
            "wonValue": 60000.00,
            "lostCount": 1,
            "lostValue": 10000.00,
            "inProgressCount": 3,
            "inProgressValue": 30000.00
          }
        ],
        "pipelines": [
          {
            "pipelineId": 12,
            "pipelineName": "Q1 Photography Pipeline",
            "wonCount": 5,
            "wonValue": 50000.00,
            "lostCount": 0,
            "lostValue": 0.00,
            "inProgressCount": 2,
            "inProgressValue": 20000.00
          }
        ]
      },
      {
        "month": 2,
        "wonCount": 15,
        "wonValue": 150000.00,
        "lostCount": 3,
        "lostValue": 30000.00,
        "inProgressCount": 10,
        "inProgressValue": 100000.00,
        "categories": [],
        "users": [],
        "pipelines": []
      }
      // ... months 3-12
    ]
  }
}
```

**Field Descriptions:**
| Field | Type | Description |
|-------|------|-------------|
| `year` | Integer | The year for which data is returned |
| `months` | Array | Array of 12 month objects (1-12) |
| `months[].month` | Integer | Month number (1-12) |
| `months[].wonCount` | Long | Number of WON deals in this month |
| `months[].wonValue` | BigDecimal | Total value of WON deals in this month |
| `months[].lostCount` | Long | Number of LOST deals in this month |
| `months[].lostValue` | BigDecimal | Total value of LOST deals in this month |
| `months[].inProgressCount` | Long | Number of IN_PROGRESS deals in this month |
| `months[].inProgressValue` | BigDecimal | Total value of IN_PROGRESS deals in this month |
| `months[].categories` | Array | Optional breakdown by organization category for this month |
| `months[].users` | Array | Optional breakdown by user for this month |
| `months[].pipelines` | Array | Optional breakdown by pipeline for this month |

**Note:** For WON deals, uses `won_at` timestamp (fallback to `updated_at`/`created_at`). For LOST deals, uses `lost_at` timestamp (fallback to `updated_at`/`created_at`). For IN_PROGRESS, uses `updated_at`/`created_at`.

---

### 5b. Get Deal Status Monthly by Sales User

**GET** `/api/category-manager/dashboard/deals-status-monthly-by-user`

Returns deal status (WON/LOST/IN_PROGRESS) **per sales user** with a full 12-month breakdown for the year. User-first view: each user has an array of 12 months. Scoped to the Category Manager's hierarchy.

**Query Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `year` | Integer | Yes | Year (e.g., `2026`) |

**Response:** `DealStatusMonthlyByUserResponse`

**Example Request:**
```http
GET /api/category-manager/dashboard/deals-status-monthly-by-user?year=2026
Authorization: Bearer <token>
```

**Example Response (200 OK):**
```json
{
  "success": true,
  "message": "Deal status monthly by user fetched",
  "data": {
    "year": 2026,
    "users": [
      {
        "userId": 5,
        "userName": "Jane Doe",
        "email": "jane.doe@brideside.com",
        "months": [
          { "month": 1, "wonCount": 2, "wonValue": 20000.00, "lostCount": 0, "lostValue": 0.00, "inProgressCount": 3, "inProgressValue": 30000.00 },
          { "month": 2, "wonCount": 4, "wonValue": 40000.00, "lostCount": 1, "lostValue": 10000.00, "inProgressCount": 2, "inProgressValue": 20000.00 },
          { "month": 3, "wonCount": 0, "wonValue": 0.00, "lostCount": 0, "lostValue": 0.00, "inProgressCount": 5, "inProgressValue": 50000.00 }
          // ... months 4-12
        ]
      },
      {
        "userId": 8,
        "userName": "John Smith",
        "email": "john.smith@brideside.com",
        "months": [ /* 12 months */ ]
      }
    ]
  }
}
```

**Field Descriptions:**
| Field | Type | Description |
|-------|------|-------------|
| `year` | Integer | The year for which data is returned |
| `users` | Array | One entry per sales user in the Category Manager's hierarchy |
| `users[].userId` | Long | User ID (organization owner) |
| `users[].userName` | String | User full name |
| `users[].email` | String | User email |
| `users[].months` | Array | Exactly 12 objects (month 1–12) with counts/values for that user |
| `users[].months[].month` | Integer | Month (1–12) |
| `users[].months[].wonCount` | Long | WON deals for this user in this month |
| `users[].months[].wonValue` | BigDecimal | Total WON value for this user in this month |
| `users[].months[].lostCount` | Long | LOST deals for this user in this month |
| `users[].months[].lostValue` | BigDecimal | Total LOST value for this user in this month |
| `users[].months[].inProgressCount` | Long | IN_PROGRESS deals for this user in this month |
| `users[].months[].inProgressValue` | BigDecimal | Total IN_PROGRESS value for this user in this month |

---

### 5c. Get Deal Status Monthly by Pipeline

**GET** `/api/category-manager/dashboard/deals-status-monthly-by-pipeline`

Returns deal status (WON/LOST/IN_PROGRESS) **per pipeline** with a full 12-month breakdown for the year. Pipeline-first view: each pipeline has an array of 12 months. Scoped to the Category Manager's hierarchy.

**Query Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `year` | Integer | Yes | Year (e.g., `2026`) |

**Response:** `DealStatusMonthlyByPipelineResponse`

**Example Request:**
```http
GET /api/category-manager/dashboard/deals-status-monthly-by-pipeline?year=2026
Authorization: Bearer <token>
```

**Example Response (200 OK):**
```json
{
  "success": true,
  "message": "Deal status monthly by pipeline fetched",
  "data": {
    "year": 2026,
    "pipelines": [
      {
        "pipelineId": 12,
        "pipelineName": "Q1 Photography Pipeline",
        "months": [
          { "month": 1, "wonCount": 3, "wonValue": 30000.00, "lostCount": 1, "lostValue": 10000.00, "inProgressCount": 4, "inProgressValue": 40000.00 },
          { "month": 2, "wonCount": 5, "wonValue": 50000.00, "lostCount": 2, "lostValue": 20000.00, "inProgressCount": 2, "inProgressValue": 20000.00 },
          { "month": 3, "wonCount": 0, "wonValue": 0.00, "lostCount": 0, "lostValue": 0.00, "inProgressCount": 6, "inProgressValue": 60000.00 }
        ]
      },
      {
        "pipelineId": 15,
        "pipelineName": "Makeup Pipeline",
        "months": [ /* 12 months */ ]
      }
    ]
  }
}
```

**Field Descriptions:**
| Field | Type | Description |
|-------|------|-------------|
| `year` | Integer | The year for which data is returned |
| `pipelines` | Array | One entry per pipeline that has deals in the Category Manager's scope |
| `pipelines[].pipelineId` | Long | Pipeline ID |
| `pipelines[].pipelineName` | String | Pipeline name |
| `pipelines[].months` | Array | Exactly 12 objects (month 1–12) with counts/values for that pipeline |
| `pipelines[].months[].month` | Integer | Month (1–12) |
| `pipelines[].months[].wonCount` | Long | WON deals for this pipeline in this month |
| `pipelines[].months[].wonValue` | BigDecimal | Total WON value for this pipeline in this month |
| `pipelines[].months[].lostCount` | Long | LOST deals for this pipeline in this month |
| `pipelines[].months[].lostValue` | BigDecimal | Total LOST value for this pipeline in this month |
| `pipelines[].months[].inProgressCount` | Long | IN_PROGRESS deals for this pipeline in this month |
| `pipelines[].months[].inProgressValue` | BigDecimal | Total IN_PROGRESS value for this pipeline in this month |

---

### 6. Get Lost Deal Reasons Summary

**GET** `/api/category-manager/dashboard/lost-reasons`

Returns LOST deal reasons with counts and percentages, useful for donut charts. Supports optional filters.

**Query Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `category` | String | No | Filter by organization category (e.g., `"Photography"`, `"Makeup"`, `"Planning and Decor"`) |
| `userId` | Long | No | Filter by user ID (must be in Category Manager's scope) |
| `pipelineId` | Long | No | Filter by pipeline ID |

**Response:** `LostReasonSummaryResponse`

**Example Request:**
```javascript
GET /api/category-manager/dashboard/lost-reasons?category=Photography
Authorization: Bearer <token>
```

**Example Response (200 OK):**
```json
{
  "success": true,
  "message": "Lost reason summary fetched",
  "data": {
    "totalLostDeals": 28,
    "category": "Photography",
    "userId": null,
    "pipelineId": null,
    "reasons": [
      {
        "reason": "Budget",
        "count": 12,
        "percentage": 42.86
      },
      {
        "reason": "Timing",
        "count": 8,
        "percentage": 28.57
      },
      {
        "reason": "Competitor",
        "count": 5,
        "percentage": 17.86
      },
      {
        "reason": "Other",
        "count": 3,
        "percentage": 10.71
      }
    ]
  }
}
```

**Field Descriptions:**
| Field | Type | Description |
|-------|------|-------------|
| `totalLostDeals` | Long | Total number of LOST deals (after filters) |
| `category` | String | Organization category filter applied (null if none) |
| `userId` | Long | User ID filter applied (null if none) |
| `pipelineId` | Long | Pipeline ID filter applied (null if none) |
| `reasons` | Array | List of lost reasons with counts and percentages |
| `reasons[].reason` | String | Display name of the lost reason |
| `reasons[].count` | Long | Number of LOST deals with this reason |
| `reasons[].percentage` | BigDecimal | Percentage of total lost deals (0-100) |

**Note:** Percentages are calculated as `(count / totalLostDeals) * 100`, rounded to 2 decimal places.

---

### 6a. Get Lost Reasons by Pipeline

**GET** `/api/category-manager/dashboard/lost-reasons-by-pipeline`

Returns LOST deal reasons **grouped by pipeline**. Each pipeline has its own list of reasons with counts and percentages (percentages are relative to that pipeline’s total lost deals). Scoped to the Category Manager’s hierarchy.

**Query Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `category` | String | No | Filter by organization category (e.g., `"Photography"`) |

**Response:** `LostReasonsByPipelineResponse`

**Example Request:**
```http
GET /api/category-manager/dashboard/lost-reasons-by-pipeline
GET /api/category-manager/dashboard/lost-reasons-by-pipeline?category=Photography
Authorization: Bearer <token>
```

**Example Response (200 OK):**
```json
{
  "success": true,
  "message": "Lost reasons by pipeline fetched",
  "data": {
    "category": null,
    "pipelines": [
      {
        "pipelineId": 12,
        "pipelineName": "Q1 Photography Pipeline",
        "totalLostDeals": 10,
        "reasons": [
          { "reason": "Budget", "count": 5, "percentage": 50.00 },
          { "reason": "Timing", "count": 3, "percentage": 30.00 },
          { "reason": "Competitor", "count": 2, "percentage": 20.00 }
        ]
      },
      {
        "pipelineId": 15,
        "pipelineName": "Makeup Pipeline",
        "totalLostDeals": 6,
        "reasons": [
          { "reason": "Budget", "count": 4, "percentage": 66.67 },
          { "reason": "Other", "count": 2, "percentage": 33.33 }
        ]
      }
    ]
  }
}
```

**Field Descriptions:**
| Field | Type | Description |
|-------|------|-------------|
| `category` | String | Organization category filter applied (null if none) |
| `pipelines` | Array | One entry per pipeline that has LOST deals in scope |
| `pipelines[].pipelineId` | Long | Pipeline ID |
| `pipelines[].pipelineName` | String | Pipeline name |
| `pipelines[].totalLostDeals` | Long | Total LOST deals in this pipeline |
| `pipelines[].reasons` | Array | Reason breakdown for this pipeline (count + % of this pipeline’s total) |

---

### 6b. Get Lost Reasons by User

**GET** `/api/category-manager/dashboard/lost-reasons-by-user`

Returns LOST deal reasons **grouped by sales user**. Each user has their own list of reasons with counts and percentages (percentages are relative to that user’s total lost deals). Scoped to the Category Manager’s hierarchy.

**Query Parameters:**
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `category` | String | No | Filter by organization category (e.g., `"Photography"`) |

**Response:** `LostReasonsByUserResponse`

**Example Request:**
```http
GET /api/category-manager/dashboard/lost-reasons-by-user
GET /api/category-manager/dashboard/lost-reasons-by-user?category=Makeup
Authorization: Bearer <token>
```

**Example Response (200 OK):**
```json
{
  "success": true,
  "message": "Lost reasons by user fetched",
  "data": {
    "category": null,
    "users": [
      {
        "userId": 5,
        "userName": "Jane Doe",
        "email": "jane.doe@brideside.com",
        "totalLostDeals": 14,
        "reasons": [
          { "reason": "Budget", "count": 7, "percentage": 50.00 },
          { "reason": "Timing", "count": 4, "percentage": 28.57 },
          { "reason": "Competitor", "count": 3, "percentage": 21.43 }
        ]
      },
      {
        "userId": 8,
        "userName": "John Smith",
        "email": "john.smith@brideside.com",
        "totalLostDeals": 9,
        "reasons": [
          { "reason": "Budget", "count": 5, "percentage": 55.56 },
          { "reason": "Other", "count": 4, "percentage": 44.44 }
        ]
      }
    ]
  }
}
```

**Field Descriptions:**
| Field | Type | Description |
|-------|------|-------------|
| `category` | String | Organization category filter applied (null if none) |
| `users` | Array | One entry per sales user who has LOST deals in scope |
| `users[].userId` | Long | User ID (organization owner) |
| `users[].userName` | String | User full name |
| `users[].email` | String | User email |
| `users[].totalLostDeals` | Long | Total LOST deals for this user |
| `users[].reasons` | Array | Reason breakdown for this user (count + % of this user’s total) |

---

### 7. Get Sales Users and Their Pipelines

**GET** `/api/category-manager/dashboard/sales-pipelines`

Returns the **SALES users** in the Category Manager’s hierarchy and the **pipelines they own** (via `pipeline.organization.owner`). Useful for building “Sales → Pipelines” filters.

**Query Parameters:**  
None.

**Response:** `SalesPipelinesResponse`

**Example Request:**
```http
GET /api/category-manager/dashboard/sales-pipelines
Authorization: Bearer <token>
```

**Example Response (200 OK):**
```json
{
  "success": true,
  "message": "Sales users with pipelines fetched",
  "data": {
    "users": [
      {
        "userId": 5,
        "userName": "Jane Doe",
        "email": "jane.doe@brideside.com",
        "pipelines": [
          { "pipelineId": 12, "pipelineName": "Q1 Photography Pipeline" },
          { "pipelineId": 15, "pipelineName": "Makeup Pipeline" }
        ]
      },
      {
        "userId": 8,
        "userName": "John Smith",
        "email": "john.smith@brideside.com",
        "pipelines": [
          { "pipelineId": 20, "pipelineName": "Planning & Decor Pipeline" }
        ]
      }
    ]
  }
}
```

**Field Descriptions:**
| Field | Type | Description |
|-------|------|-------------|
| `users` | Array | One entry per SALES user in the Category Manager’s hierarchy who owns at least one pipeline |
| `users[].userId` | Long | SALES user ID (organization owner) |
| `users[].userName` | String | SALES user full name |
| `users[].email` | String | SALES user email |
| `users[].pipelines` | Array | Pipelines owned by this SALES user |
| `users[].pipelines[].pipelineId` | Long | Pipeline ID |
| `users[].pipelines[].pipelineName` | String | Pipeline name |

---

## Data Models

### SummaryResponse
```typescript
interface SummaryResponse {
  salesManagersCount: number;
  presalesCount: number;
  totalTeamSize: number;
  totalWonDeals: number;
  totalLostDeals: number;
  totalInProgressDeals: number;
  totalWonValue: number;      // BigDecimal as number
  totalWonValueYtd: number;   // BigDecimal as number
}
```

### RevenueSummaryResponse
```typescript
interface RevenueSummaryResponse {
  dateFrom: string;           // ISO date string
  dateTo: string;             // ISO date string
  totalDeals: number;
  totalDealValue: number;      // BigDecimal as number
  categories: RevenueByCategoryRow[];
  users: RevenueByUserRow[];
  pipelines: RevenueByPipelineRow[];
}

interface RevenueByCategoryRow {
  category: string;
  totalDeals: number;
  totalDealValue: number;
}

interface RevenueByUserRow {
  userId: number;
  userName: string;
  email: string;
  totalDeals: number;
  totalDealValue: number;
}

interface RevenueByPipelineRow {
  pipelineId: number;
  pipelineName: string;
  totalDeals: number;
  totalDealValue: number;
}
```

### WonDealsBySalesUserResponse / LostDealsBySalesUserResponse
```typescript
interface WonDealsBySalesUserResponse {
  users: SalesUserDealsRow[];
}

interface SalesUserDealsRow {
  userId: number;
  userName: string;
  email: string;
  totalDeals: number;
  totalDealValue: number;
}
```

### DealStatusMonthlySummaryResponse
```typescript
interface DealStatusMonthlySummaryResponse {
  year: number;
  months: DealStatusMonthlyRow[];
}

interface DealStatusMonthlyRow {
  month: number;              // 1-12
  wonCount: number;
  wonValue: number;
  lostCount: number;
  lostValue: number;
  inProgressCount: number;
  inProgressValue: number;
  categories?: DealStatusCategoryRow[];
  users?: DealStatusMonthlyUserRow[];
  pipelines?: DealStatusPipelineRow[];
}

interface DealStatusCategoryRow {
  category: string;
  wonCount: number;
  wonValue: number;
  lostCount: number;
  lostValue: number;
  inProgressCount: number;
  inProgressValue: number;
}

interface DealStatusMonthlyUserRow {
  userId: number;
  userName: string;
  email: string;
  wonCount: number;
  wonValue: number;
  lostCount: number;
  lostValue: number;
  inProgressCount: number;
  inProgressValue: number;
}

interface DealStatusPipelineRow {
  pipelineId: number;
  pipelineName: string;
  wonCount: number;
  wonValue: number;
  lostCount: number;
  lostValue: number;
  inProgressCount: number;
  inProgressValue: number;
}
```

### LostReasonSummaryResponse
```typescript
interface LostReasonSummaryResponse {
  totalLostDeals: number;
  category: string | null;
  userId: number | null;
  pipelineId: number | null;
  reasons: LostReasonRow[];
}

interface LostReasonRow {
  reason: string;
  count: number;
  percentage: number;         // 0-100
}
```

---

## Frontend Implementation Guide

### 1. Dashboard Summary Card

**Use Case:** Display key metrics at the top of the dashboard.

**API Call:**
```javascript
async function loadDashboardSummary() {
  const response = await fetch('/api/category-manager/dashboard/summary', {
    headers: {
      'Authorization': `Bearer ${getToken()}`
    }
  });
  const json = await response.json();
  if (json.success) {
    const data = json.data;
    // Update UI with:
    // - data.salesManagersCount
    // - data.presalesCount
    // - data.totalTeamSize
    // - data.totalWonDeals, data.totalLostDeals, data.totalInProgressDeals
    // - Format currency: data.totalWonValue, data.totalWonValueYtd
  }
}
```

**UI Suggestions:**
- Display stats in cards/grid layout
- Format currency values (e.g., `$1,250,000.50`)
- Use color coding: green for won deals, red for lost deals, blue for in progress

---

### 2. Revenue by Category Section

**Use Case:** Show revenue breakdown by organization category, user, and pipeline.

**API Call:**
```javascript
async function loadRevenue(dateFrom, dateTo) {
  const params = new URLSearchParams({
    dateFrom: dateFrom,  // e.g., '2025-01-01'
    dateTo: dateTo       // e.g., '2025-12-31'
  });
  const response = await fetch(`/api/category-manager/dashboard/revenue?${params}`, {
    headers: {
      'Authorization': `Bearer ${getToken()}`
    }
  });
  const json = await response.json();
  if (json.success) {
    const data = json.data;
    // Display:
    // - data.categories (table or chart)
    // - data.users (table)
    // - data.pipelines (table)
  }
}
```

**UI Suggestions:**
- Default date range: Start of current year to today
- Allow user to change date range with date pickers
- Display in tables with sortable columns
- Consider bar/pie charts for category breakdown
- Format currency values consistently

---

### 3. Monthly Deal Status Chart

**Use Case:** Show monthly trends for WON/LOST/IN_PROGRESS deals.

**API Call:**
```javascript
async function loadMonthlyStatus(year) {
  const response = await fetch(`/api/category-manager/dashboard/deals-status-monthly?year=${year}`, {
    headers: {
      'Authorization': `Bearer ${getToken()}`
    }
  });
  const json = await response.json();
  if (json.success) {
    const data = json.data;
    // Display:
    // - Line/bar chart with months on x-axis, counts/values on y-axis
    // - Three series: WON, LOST, IN_PROGRESS
    // - Optional: Drill down to category/user/pipeline breakdown for a month
  }
}
```

**UI Suggestions:**
- Year selector dropdown (default: current year)
- Line chart or grouped bar chart
- Tooltip showing exact values on hover
- Click on a month to show detailed breakdown (categories/users/pipelines)

---

### 4. Lost Deal Reasons Donut Chart

**Use Case:** Visualize why deals are being lost.

**API Call:**
```javascript
async function loadLostReasons(filters = {}) {
  const params = new URLSearchParams();
  if (filters.category) params.append('category', filters.category);
  if (filters.userId) params.append('userId', filters.userId);
  if (filters.pipelineId) params.append('pipelineId', filters.pipelineId);
  
  const response = await fetch(`/api/category-manager/dashboard/lost-reasons?${params}`, {
    headers: {
      'Authorization': `Bearer ${getToken()}`
    }
  });
  const json = await response.json();
  if (json.success) {
    const data = json.data;
    // Display:
    // - Donut/pie chart with reasons
    // - Legend showing reason name, count, and percentage
  }
}
```

**UI Suggestions:**
- Donut or pie chart
- Color-coded segments
- Legend with reason name, count, and percentage
- Optional filters: category dropdown, user dropdown, pipeline dropdown

---

## Examples

### Complete Dashboard Page Example

```javascript
// category-manager-dashboard.js

const API_BASE_URL = '/api/category-manager/dashboard';

async function loadDashboard() {
  try {
    // Load summary stats
    const summaryResponse = await fetch(`${API_BASE_URL}/summary`, {
      headers: { 'Authorization': `Bearer ${getToken()}` }
    });
    const summaryData = (await summaryResponse.json()).data;
    updateSummaryCards(summaryData);

    // Load revenue (default: current year)
    const now = new Date();
    const yearStart = new Date(now.getFullYear(), 0, 1);
    const dateFrom = yearStart.toISOString().split('T')[0];
    const dateTo = now.toISOString().split('T')[0];
    await loadRevenue(dateFrom, dateTo);

    // Load monthly status (current year)
    await loadMonthlyStatus(now.getFullYear());

    // Load lost reasons
    await loadLostReasons();
  } catch (error) {
    console.error('Error loading dashboard:', error);
    showError('Failed to load dashboard data');
  }
}

function updateSummaryCards(data) {
  document.getElementById('salesManagersCount').textContent = data.salesManagersCount || 0;
  document.getElementById('presalesCount').textContent = data.presalesCount || 0;
  document.getElementById('teamSize').textContent = data.totalTeamSize || 0;
  document.getElementById('wonDeals').textContent = data.totalWonDeals || 0;
  document.getElementById('lostDeals').textContent = data.totalLostDeals || 0;
  document.getElementById('inProgressDeals').textContent = data.totalInProgressDeals || 0;
  document.getElementById('totalRevenue').textContent = formatCurrency(data.totalWonValue);
  document.getElementById('revenueYtd').textContent = formatCurrency(data.totalWonValueYtd);
}

function formatCurrency(value) {
  if (value == null) return '-';
  return new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD',
    minimumFractionDigits: 2
  }).format(value);
}

// Initialize on page load
document.addEventListener('DOMContentLoaded', loadDashboard);
```

---

## Error Handling

All endpoints return standard error responses:

**400 Bad Request:**
```json
{
  "success": false,
  "message": "year is required"
}
```

**401 Unauthorized:**
```json
{
  "success": false,
  "message": "User not authenticated"
}
```

**403 Forbidden:**
```json
{
  "success": false,
  "message": "Access denied"
}
```

**500 Internal Server Error:**
```json
{
  "success": false,
  "message": "Internal server error"
}
```

**Frontend should:**
- Check `response.ok` or `response.status` before parsing JSON
- Display error messages from the `message` field
- Handle network errors gracefully
- Show loading states while fetching data

---

## Notes

1. **All data is pre-filtered**: The backend automatically scopes all data to the Category Manager's hierarchy. No client-side filtering needed.

2. **Deal attribution**: Deals are attributed via `pipeline → organization → owner`. Only deals where the organization owner is in the Category Manager's scope are included.

3. **Date handling**: Use ISO 8601 date format (`YYYY-MM-DD`) for date parameters.

4. **BigDecimal values**: All monetary values are returned as numbers (JSON doesn't have BigDecimal). Frontend should format them as currency.

5. **Empty arrays**: If no data matches the filters, arrays will be empty (`[]`), not `null`.

6. **Performance**: These endpoints query the database, so consider caching or debouncing rapid requests.

7. **Role enforcement**: All endpoints require `CATEGORY_MANAGER` role. If a user without this role tries to access, they'll get a 403 Forbidden response.

---

## Integration Checklist

- [ ] Add authentication token to all requests
- [ ] Implement error handling for all API calls
- [ ] Format currency values consistently
- [ ] Handle empty data states (no deals, no users, etc.)
- [ ] Add loading indicators while fetching data
- [ ] Implement date range picker for revenue endpoint
- [ ] Add year selector for monthly status endpoint
- [ ] Create charts/graphs for visualizations (revenue by category, monthly trends, lost reasons)
- [ ] Test with Category Manager user account
- [ ] Verify data is scoped correctly (only shows Category Manager's team data)
