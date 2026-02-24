# Frontend changes: make Activity highlight instant

## Background

Today, when the UI receives an `activityId` (deep link / “open activity”), it tries to locate the row by:

1. Loading page 0 (e.g. 200 items)
2. Searching for `activityId`
3. If not found, loading page 1, then page 2, etc.

This is slow for older activities because it requires **many network calls** and **many full list queries**.

## Goal

Make highlighting **O(1) / O(2) API calls**, while keeping the highlighted row consistent with:

- current filters
- current sort
- current page size

## Backend endpoints available

### 1) Fetch single activity (Option A)

- `GET /api/activities/{id}`
- Returns: one `ActivityDTO` (same shape as list items)
- Auth/scoping: matches `GET /api/activities` (won’t return activities outside user scope)
- Errors:
  - `404` if not found **or not visible under scoping**
  - `401/403` if unauthenticated/forbidden

### 2) Jump-to page for “highlight inside table” (Option B)

- `GET /api/activities/jump`
- Required query params:
  - `activityId` (number)
  - `size` (number) — must match table page size (e.g. 200)
- Optional query params (must match list endpoint exactly):
  - `sort` (string, can be repeated): `sort=dueDate,desc` (Spring format)
  - all list filters: `personId`, `organizationId`, `assignedUserId`, `serviceCategory`, `organizationCategory`, `dateFrom`, `dateTo`, `assignedUser`, `category`, `status`, `done`, `dealId`
- Response:
  - `page` (0-based)
  - `size`
  - `indexInPage` (0..size-1)
  - `activity` (`ActivityDTO`) — optional instant highlight while list page loads

## Frontend implementation (recommended)

### Option A (best UX): highlight without paging search

Use this when the UI can highlight/open the activity **without requiring it to be present in the currently loaded page**.

**Flow**

1. Read `activityId` from URL / state.
2. Call `GET /api/activities/{id}` once.
3. If 200 OK:
   - open details drawer / side panel using the returned DTO
   - mark it as “selected/highlighted” in UI state
4. Load the table normally (page 0 etc.) for browsing.
5. If 404:
   - show: “Activity not found (or you don’t have access)”
   - clear the deep-link selection

### Option B (best for “highlight row inside the table”): jump + load page

Use this when the UX must scroll to the row **inside the paginated table**.

**Flow**

1. Build the same query params that the table uses (filters + sort + size).
2. Call `/api/activities/jump` to get the page number (0-based).
3. Load exactly that page from `/api/activities?page=...&size=...&sort=...&<same filters>`
4. Highlight the row (prefer matching by `activityId`, `indexInPage` is a helper)

**Important**

- The `sort`, `size`, and all filters must be **identical** in both calls, otherwise the page/index may not match the table ordering.
- After implementing this, **delete/disable** the old “fetch next page until found” loop.

## TypeScript examples

### Build query params (handles array filters + repeated sort)

```ts
type SortParam = string; // e.g. "dueDate,desc"

type ActivityFilters = {
  personId?: number;
  organizationId?: number | number[];
  assignedUserId?: number | number[];
  serviceCategory?: string;
  organizationCategory?: string;
  dateFrom?: string; // yyyy-MM-dd
  dateTo?: string;   // yyyy-MM-dd
  assignedUser?: string;
  category?: string;
  status?: string;
  done?: boolean;
  dealId?: number;
};

function toSearchParams(filters: ActivityFilters, opts: { sort?: SortParam[]; page?: number; size?: number }) {
  const sp = new URLSearchParams();

  const add = (k: string, v: unknown) => {
    if (v === undefined || v === null) return;
    if (typeof v === "string" && v.trim() === "") return;
    sp.append(k, String(v));
  };

  // Filters (arrays become repeated query params: organizationId=1&organizationId=2)
  const appendMaybeArray = (k: string, v?: number | number[]) => {
    if (v === undefined || v === null) return;
    if (Array.isArray(v)) v.forEach(x => add(k, x));
    else add(k, v);
  };

  add("personId", filters.personId);
  appendMaybeArray("organizationId", filters.organizationId);
  appendMaybeArray("assignedUserId", filters.assignedUserId);
  add("serviceCategory", filters.serviceCategory);
  add("organizationCategory", filters.organizationCategory);
  add("dateFrom", filters.dateFrom);
  add("dateTo", filters.dateTo);
  add("assignedUser", filters.assignedUser);
  add("category", filters.category);
  add("status", filters.status);
  add("done", filters.done);
  add("dealId", filters.dealId);

  // Sort can be repeated: sort=dueDate,desc&sort=id,desc
  (opts.sort ?? []).forEach(s => add("sort", s));

  add("page", opts.page);
  add("size", opts.size);

  return sp;
}
```

### Option B: jump + load page (no paging loop)

```ts
async function highlightActivityInTable(activityId: number, filters: ActivityFilters, table: { size: number; sort: string[] }) {
  const jumpParams = toSearchParams(filters, { sort: table.sort });
  jumpParams.set("activityId", String(activityId));
  jumpParams.set("size", String(table.size));

  const jumpRes = await fetch(`/api/activities/jump?${jumpParams.toString()}`);
  if (jumpRes.status === 404) {
    // show toast: not found or not visible in current filters
    return;
  }
  if (!jumpRes.ok) throw new Error(await jumpRes.text());

  const jump = await jumpRes.json() as { page: number; size: number; indexInPage: number; activity?: { id: number } };

  const listParams = toSearchParams(filters, { sort: table.sort, page: jump.page, size: jump.size });
  const listRes = await fetch(`/api/activities?${listParams.toString()}`);
  if (!listRes.ok) throw new Error(await listRes.text());

  const pageData = await listRes.json() as { content: Array<{ id: number }>; number: number; size: number };

  // Highlight row (prefer matching by id; indexInPage is just a helper)
  const idx = pageData.content.findIndex(x => x.id === activityId);
  const indexToHighlight = idx >= 0 ? idx : jump.indexInPage;

  // setSelectedActivityId(activityId); scrollRowIntoView(indexToHighlight);
}
```

## UX notes

- If `/jump` returns 404, show: **“Activity not found in current filters”** and offer a “Clear filters” action.
- Always show a small loading indicator while resolving a deep-link selection.

