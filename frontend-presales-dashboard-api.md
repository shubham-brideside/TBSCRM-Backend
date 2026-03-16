# Presales dashboard API

Base path: **`/api/presales/dashboard`**

All routes require authentication and role **`PRESALES`**. Responses use the same `ApiResponse<T>` envelope and DTOs as the sales dashboard (`SalesDashboardDtos`).

## Scoping

| Area | Scope |
|------|--------|
| Deals, revenue, lost reasons, pipeline performance | Deals whose organization owner is the presales user’s **Sales manager** (same as manager’s pipelines). If the user has no manager, deal lists are empty. |
| Activities monthly | Activities **assigned to** the presales user. |
| Target vs achievement | Presales user’s **own** targets vs WON deals in manager-org scope. |

## Endpoints

| Method | Path | Query params | Description |
|--------|------|--------------|-------------|
| GET | `/summary` | — | Summary counts, values, commission, YTD won, activity stats. |
| GET | `/deals-status-monthly` | `year` (required) | WON/LOST/IN_PROGRESS per month. |
| GET | `/revenue` | `dateFrom`, `dateTo` (ISO date) | WON revenue by pipeline. |
| GET | `/lost-reasons` | `category`, `pipelineId` optional | Lost reason breakdown. |
| GET | `/lost-reasons-by-organization` | `category` optional | Lost reasons grouped by org. |
| GET | `/lost-reasons-by-pipeline` | `category`, `pipelineId` optional | Lost reasons grouped by pipeline. |
| GET | `/lost-deals-by-stage-per-organization` | `category`, `pipelineId` optional | LOST deals by stage per org. |
| GET | `/lost-deals-by-stage` | `category`, `pipelineId` optional | LOST deals aggregated by pipeline + stage (single flat list). |
| GET | `/activities-monthly` | `year` (required) | Calls/meetings per month for current user. |
| GET | `/pipeline-performance` | — | Per-pipeline status counts/values. |
| GET | `/target-vs-achievement` | `year` (required) | Targets vs achieved WON revenue. |

### Example

```http
GET /api/presales/dashboard/summary
Authorization: Bearer <token>
```

```http
GET /api/presales/dashboard/deals-status-monthly?year=2025
```

Non-PRESALES users receive **403** on these paths. Use `/api/sales/dashboard/*` for SALES users (own organizations only).
