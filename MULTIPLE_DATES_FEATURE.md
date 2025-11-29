# Multiple Dates Feature for Deals

## Overview

This feature enables deals to support multiple event dates instead of a single date. Each date can have its own corresponding Google Calendar event, allowing for better management of multi-day events or events with multiple date options.

## Database Changes

### Migration Files

#### 1. `V20251226_01__deals_change_event_date_to_multiple.sql`
- Adds `event_dates` JSON column to store an array of dates
- Migrates existing `event_date` values to the new JSON array format
- Keeps `event_date` column for backward compatibility

#### 2. `V20251226_02__deals_add_multiple_calendar_event_ids.sql`
- Adds `google_calendar_event_ids` JSON column to store a mapping of dates to Google Calendar event IDs
- Migrates existing single `google_calendar_event_id` to the new format
- Format: `{"2024-01-01": "event_id_1", "2024-01-02": "event_id_2"}`

### Schema Changes

**Deals Table:**
- `event_date` (DATE) - **Legacy field**, kept for backward compatibility
- `event_dates` (JSON) - **New field**, stores array of date strings: `["2024-01-01", "2024-01-02"]`
- `google_calendar_event_id` (VARCHAR) - **Legacy field**, kept for backward compatibility
- `google_calendar_event_ids` (JSON) - **New field**, stores mapping: `{"2024-01-01": "id1", "2024-01-02": "id2"}`

## API Changes

### Request DTOs

#### CreateRequest / UpdateRequest

**New Field:**
```json
{
  "eventDates": ["2024-01-01", "2024-01-02", "2024-01-03"]
}
```

**Legacy Support:**
```json
{
  "eventDate": "2024-01-01"
}
```

Both fields are supported. If `eventDates` is provided, it takes precedence. If only `eventDate` is provided, it will be converted to `eventDates` format.

### Response DTO

#### DealResponse

**New Field:**
```json
{
  "eventDates": ["2024-01-01", "2024-01-02", "2024-01-03"],
  "eventDate": "2024-01-01"  // Legacy field, contains first date
}
```

## Implementation Details

### Entity Layer

**Deal.java:**
- `eventDate` (LocalDate) - Legacy field, kept for backward compatibility
- `eventDates` (String) - JSON array of date strings
- `googleCalendarEventId` (String) - Legacy field
- `googleCalendarEventIds` (String) - JSON object mapping dates to event IDs

### Service Layer

**DealServiceImpl.java:**

Key methods:
- `getFirstEventDate(Deal)` - Gets the first date for sorting/legacy compatibility
- `parseEventDates(Deal)` - Parses JSON to List<LocalDate>
- `eventDatesToJson(List<String>)` - Converts list to JSON string
- `eventIdsMapToJson(Map<String, String>)` - Converts event IDs map to JSON

**Sorting:**
- When sorting by `eventDate`, the system uses the first date from `eventDates`
- Maintains backward compatibility with existing sorting functionality

### Calendar Integration

**GoogleCalendarService.java:**

**New Methods:**
- `upsertDealEvents(Deal)` - Creates/updates multiple calendar events (one per date)
- `deleteDealEvents(Deal)` - Deletes all calendar events for a deal
- `getAllEventDates(Deal)` - Gets all dates from eventDates or falls back to eventDate
- `parseEventIds(Deal)` - Parses the event IDs mapping

**Behavior:**
- Creates one Google Calendar event per date in `eventDates`
- Each event is tracked independently with its own event ID
- When dates are removed, corresponding calendar events are automatically deleted
- When dates are added, new calendar events are created
- Event summaries include the specific date when there are multiple dates

**Legacy Methods (Deprecated):**
- `upsertDealEvent(Deal)` - Uses first date only
- `deleteDealEvent(Deal)` - Deletes single event

## Usage Examples

### Creating a Deal with Multiple Dates

**Request:**
```http
POST /api/deals
Content-Type: application/json

{
  "name": "Wedding Photography",
  "value": 5000.00,
  "eventDates": ["2024-06-15", "2024-06-16", "2024-06-17"],
  "organizationId": 1,
  "pipelineId": 1
}
```

**Response:**
```json
{
  "id": 123,
  "name": "Wedding Photography",
  "value": 5000.00,
  "eventDates": ["2024-06-15", "2024-06-16", "2024-06-17"],
  "eventDate": "2024-06-15",
  ...
}
```

### Updating Event Dates

**Request:**
```http
PATCH /api/deals/123
Content-Type: application/json

{
  "eventDates": ["2024-06-15", "2024-06-16"]
}
```

**Result:**
- Calendar event for "2024-06-17" is automatically deleted
- Calendar events for "2024-06-15" and "2024-06-16" are updated if they exist

### Using Legacy Single Date (Backward Compatible)

**Request:**
```http
POST /api/deals
Content-Type: application/json

{
  "name": "Single Day Event",
  "value": 2000.00,
  "eventDate": "2024-07-01",
  "organizationId": 1
}
```

**Result:**
- Automatically converted to `eventDates: ["2024-07-01"]`
- Works exactly as before

## Google Calendar Integration

### Multiple Calendar Events

When a deal has multiple dates, the system creates separate calendar events:

**Example:**
- Deal with dates: `["2024-06-15", "2024-06-16", "2024-06-17"]`
- Creates 3 calendar events:
  - Event 1: "Wedding Photography • Organization Name (2024-06-15)"
  - Event 2: "Wedding Photography • Organization Name (2024-06-16)"
  - Event 3: "Wedding Photography • Organization Name (2024-06-17)"

### Event ID Tracking

The system maintains a mapping of dates to event IDs:
```json
{
  "2024-06-15": "google_event_id_1",
  "2024-06-16": "google_event_id_2",
  "2024-06-17": "google_event_id_3"
}
```

This allows:
- Individual event updates
- Individual event deletions
- Proper cleanup when dates are removed

## Migration Guide

### For Existing Deals

Existing deals with a single `event_date` are automatically migrated:
- Single date is converted to `event_dates` JSON array
- Existing `google_calendar_event_id` is mapped to the first date in `google_calendar_event_ids`

### For Frontend Applications

**Recommended Approach:**
1. Use `eventDates` array for new implementations
2. Support both `eventDate` and `eventDates` during transition
3. Display all dates from `eventDates` when available
4. Fall back to `eventDate` for legacy data

**Example Frontend Code:**
```javascript
// Get dates from deal
const dates = deal.eventDates || (deal.eventDate ? [deal.eventDate] : []);

// Display dates
dates.forEach(date => {
  console.log(`Event date: ${date}`);
});
```

## Backward Compatibility

### Supported Scenarios

1. **Legacy API calls** - Single `eventDate` still works
2. **Legacy database records** - Old `event_date` column is preserved
3. **Legacy calendar events** - Single `google_calendar_event_id` still works
4. **Mixed usage** - Can use both old and new formats simultaneously

### Migration Path

1. **Phase 1 (Current):** Both formats supported
2. **Phase 2 (Future):** Deprecate legacy fields
3. **Phase 3 (Future):** Remove legacy fields (requires migration of all data)

## Testing

### Test Scenarios

1. **Create deal with multiple dates**
   - Verify all dates are stored
   - Verify multiple calendar events are created

2. **Update dates (add/remove)**
   - Verify new events are created
   - Verify removed events are deleted

3. **Legacy single date**
   - Verify backward compatibility
   - Verify conversion to array format

4. **Sorting by eventDate**
   - Verify sorting uses first date
   - Verify null handling

5. **Calendar sync**
   - Verify all events sync correctly
   - Verify event updates work
   - Verify event deletions work

## API Endpoints

All existing endpoints work with the new format:

- `POST /api/deals` - Create deal (supports `eventDates`)
- `PATCH /api/deals/{id}` - Update deal (supports `eventDates`)
- `GET /api/deals` - List deals (returns `eventDates`)
- `GET /api/deals/{id}` - Get deal (returns `eventDates`)

## Notes

- Date format: ISO-8601 (yyyy-MM-dd)
- Dates are stored as strings in JSON arrays
- Calendar events are created/updated/deleted automatically
- Sorting uses the first date when multiple dates exist
- All operations maintain backward compatibility

## Future Enhancements

Potential improvements:
- Date range support (start date + end date)
- Recurring events support
- Date validation (e.g., no past dates)
- Bulk date operations
- Date-based filtering in API

