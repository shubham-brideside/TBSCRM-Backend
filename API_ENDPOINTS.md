# API Endpoints Overview

Quick reference of the most frequently used REST endpoints exposed by the Brideside CRM backend. All routes are prefixed with `/api` and return the standard `ApiResponse<T>` envelope unless noted otherwise.

| Domain | Method & Path | Purpose | Key Notes |
| --- | --- | --- | --- |
| **Organizations** | `POST /api/organizations` | Create a vendor/organization record | Include `name`, `ownerId`, `category`, optional `googleCalendarId`. Triggers persistence only; no calendar sync. |
|  | `GET /api/organizations` | List organizations | Returns each org including `googleCalendarId` so the UI can filter “Show only orgs with Google Calendar”. |
|  | `GET /api/organizations/{id}` | Fetch a single organization | Used on edit forms and calendar filters. |
|  | `PUT /api/organizations/{id}` | Update organization | Full update; clearing `googleCalendarId` disables vendor calendar sync for that org. |
|  | `DELETE /api/organizations/{id}` | Delete organization | Removes org and related metadata. Make sure no critical deals remain. |
|  | `GET /api/organizations/categories` | List category options | Fills dropdowns; values align with `Organization.OrganizationCategory`. |
|  | `GET /api/organizations/owners` | List eligible owners | Returns users with `SALES` or `CATEGORY_MANAGER` roles. |
| **Deals** | `POST /api/deals` | Create a deal | If `organizationId` has `googleCalendarId` and `eventDate` is set, backend upserts an all-day Google Calendar event. |
|  | `GET /api/deals` | List all deals | Primarily for CRM tables; response includes `googleCalendarEventId`. |
|  | `GET /api/deals/{id}` | Fetch deal details | Shows current stage, status, calendar event id, etc. |
|  | `GET /api/deals/won` | List won deals | Useful for revenue dashboards and confirming that “won” deals got mirrored to Google Calendar. |
|  | `GET /api/deals/lost` | List lost deals | Filtered status view. |
|  | `GET /api/deals/inprogress` | List in-progress deals | For pipeline tracking. |
|  | `GET /api/deals/person/{personId}` | Deals tied to a person | Enables person-centric dashboards. |
|  | `GET /api/deals/organization/{organizationId}` | Deals for an organization | Pairs with vendor calendar view to correlate events with deals. |
|  | `GET /api/deals/category/{categoryId}` | Deals under a category | Category performance reporting. |
|  | `PUT /api/deals/{id}/stage` | Move deal to another stage | Request body is `DealDtos.UpdateStageRequest`. Does **not** change status automatically. |
|  | `PATCH /api/deals/{id}/status` | Update deal status | Request body `{"status":"WON"|"LOST"|...}`; when status becomes `WON`, the backend re-syncs Google Calendar. |
|  | `DELETE /api/deals/{id}` | Delete deal | Also deletes the linked Google Calendar event, if any. |
| **Calendar Aggregation** | `GET /api/calendar/vendor-events` | List mirrored vendor Google Calendar events | Query params: `organizationId` (optional), `from`, `to` as ISO instants. Response merges into the CRM calendar UI so events vendors add directly in Google appear in-app. |
| **Supporting Metadata** | `GET /api/persons` | List/search persons/leads | Supports filtering by label, source, organization, owner, and lead date range. |
|  | `GET /api/persons/labels` | Enumerate person labels | Drives dropdown filters and forms. |
|  | `GET /api/persons/sources` | Enumerate lead sources | Same usage as above. |
|  | `GET /api/pipelines` | List pipelines (optionally with stages) | Use `?includeStages=true` when building Kanban boards. |
|  | `POST /api/pipelines` | Create pipeline | Accepts `PipelineRequest`; optional stage list. |
|  | `GET /api/sources` etc. | Other controllers follow the same `GET/POST/PUT/DELETE` patterns; see Swagger (`/swagger-ui.html`) for full catalog. |

## Calendar Sync Flow

1. **Vendor ➜ CRM**: `VendorCalendarSyncServiceImpl` polls every `google.calendar.poll-interval-minutes` (default 10) for each organization with a `googleCalendarId`. Events are stored in `vendor_calendar_events` and served via `/api/calendar/vendor-events`.
2. **CRM ➜ Google**: Whenever a deal with `eventDate` under a synced organization is created, updated, marked WON/LOST, or deleted, `GoogleCalendarService` creates/updates/deletes the corresponding Google event and stores the `googleCalendarEventId` on the deal.

Refer to `frontend-calendar-sync.md` for payload examples and UI guidance.


