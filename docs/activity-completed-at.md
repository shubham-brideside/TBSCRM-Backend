# Activity `completed_at` support

This backend now persists **when an Activity was marked done** in a new DB column: `activities.completed_at`.

## Do we need frontend changes?

**No**, if your frontend already toggles done by calling:

- `POST /api/activities/{id}/done?value=true`
- `POST /api/activities/{id}/done?value=false`

Because the backend sets/clears `completed_at` automatically inside this endpoint.

### Important

- The `done` field is **read-only** in `ActivityDTO` and should not be updated via `PUT /api/activities/{id}`.
- Use `POST /api/activities/{id}/done` to change done state.

## What the backend does

When `POST /api/activities/{id}/done` is called:

- If `value=true`:
  - sets `done = true`
  - sets `completed_at = now()`
  - (if category is `CALL` and `duration_minutes` is provided) saves duration
- If `value=false`:
  - sets `done = false`
  - sets `completed_at = NULL`

## API response

`ActivityDTO` now includes a read-only field:

- `completedAt` (ISO-8601 instant, e.g. `2026-02-22T10:15:30.123Z`)

Frontend can ignore it, or optionally display it / use it for reporting UI.

## Example requests

Mark done:

```bash
curl -X POST "http://localhost:8080/api/activities/123/done?value=true&duration_minutes=12"
```

Re-open:

```bash
curl -X POST "http://localhost:8080/api/activities/123/done?value=false"
```

