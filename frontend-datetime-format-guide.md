# Frontend DateTime Format Guide

This document describes the correct date/time formats for API requests. Using incorrect formats will cause JSON deserialization errors (e.g. `DateTimeParseException`).

---

## Base URL & Auth

**Base URL:** `/api/organizations`

**Auth:** `Authorization: Bearer <token>` on all endpoints

---

## DateTime Fields

The backend expects **ISO-8601** format for date-time fields.

### Correct Format

| Format | Example | Use |
|--------|---------|-----|
| `yyyy-MM-dd'T'HH:mm:ss` | `2026-03-02T00:00:00` | Local date-time |
| `yyyy-MM-dd'T'HH:mm:ss.SSS` | `2026-03-02T00:00:00.000` | With milliseconds |
| `yyyy-MM-dd'T'HH:mm:ss.SSS'Z'` | `2026-03-02T00:00:00.000Z` | UTC (also accepted) |

### Incorrect Format (will fail)

| Incorrect | Problem |
|-----------|---------|
| `2026-03-02T00:00:00T00:00:00` | Duplicate `T` and time portion |
| `2026-03-02 00:00:00` | Space instead of `T` (may fail in some contexts) |
| `02/03/2026` | Non-ISO format |
| `March 2, 2026` | Non-ISO format |

---

## Affected Fields

| Field | Endpoint | Type |
|-------|----------|------|
| `onboardingDate` | `PUT /{orgId}/vendors/{vendorId}` | LocalDateTime |
| `issuedOn` | `PUT /{orgId}/vendors/{vendorId}/assets/{assetId}` | LocalDate (date only) |

---

## Common Error

**Error message:**
```json
{
  "success": false,
  "message": "An error occurred: JSON parse error: Cannot deserialize value of type `java.time.LocalDateTime` from String \"2026-03-02T00:00:00T00:00:00\": Failed to deserialize java.time.LocalDateTime: (java.time.format.DateTimeParseException) Text '2026-03-02T00:00:00T00:00:00' could not be parsed, unparsed text found at index 19",
  "data": null
}
```

**Cause:** Duplicate time portion in the string (e.g. `T00:00:00` appended twice).

**Fix:** Send `"2026-03-02T00:00:00"` instead of `"2026-03-02T00:00:00T00:00:00"`.

---

## JavaScript / TypeScript Examples

### Correct – using Date

```ts
const date = new Date(2026, 2, 2); // March 2, 2026 (month is 0-indexed)
const onboardingDate = date.toISOString(); // "2026-03-02T00:00:00.000Z"
```

### Correct – date only (no time)

```ts
const date = new Date(2026, 2, 2);
const onboardingDate = date.toISOString().slice(0, 19); // "2026-03-02T00:00:00"
```

### Correct – manual format

```ts
function toISODateTime(date: Date): string {
  const pad = (n: number) => n.toString().padStart(2, "0");
  return `${date.getFullYear()}-${pad(date.getMonth() + 1)}-${pad(date.getDate())}T${pad(date.getHours())}:${pad(date.getMinutes())}:${pad(date.getSeconds())}`;
}
```

### Incorrect – avoid concatenating date + "T" + time when both include time

```ts
// BAD – if dateStr is "2026-03-02T00:00:00", this produces "2026-03-02T00:00:00T00:00:00"
const onboardingDate = dateStr + "T" + timeStr;
```

---

## Date-only fields (e.g. `issuedOn`)

For `LocalDate` fields, use `yyyy-MM-dd`:

| Format | Example |
|--------|---------|
| `yyyy-MM-dd` | `2026-03-02` |

```ts
const issuedOn = "2026-03-02";
// Or from Date:
const issuedOn = new Date(2026, 2, 2).toISOString().slice(0, 10);
```

---

## Related Docs

- `frontend-integration-complete.md` – Full API integration reference
- `frontend-api-integration-guide.md` – Organization Details page

---

## Summary

- Use **ISO-8601** format: `yyyy-MM-dd'T'HH:mm:ss` or `yyyy-MM-dd'T'HH:mm:ss.SSS`
- Avoid duplicate `T` or time portions
- Use `Date.toISOString()` or equivalent for consistent formatting
