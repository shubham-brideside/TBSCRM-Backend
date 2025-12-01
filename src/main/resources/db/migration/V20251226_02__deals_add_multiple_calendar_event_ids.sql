-- Add google_calendar_event_ids column to store multiple calendar event IDs
-- This maps each event date to its corresponding Google Calendar event ID
ALTER TABLE deals 
ADD COLUMN IF NOT EXISTS google_calendar_event_ids JSON NULL;

-- Migrate existing single google_calendar_event_id to google_calendar_event_ids JSON object
-- If google_calendar_event_id exists, create a mapping to the first available date
UPDATE deals 
SET google_calendar_event_ids = CASE 
    WHEN google_calendar_event_id IS NOT NULL AND event_dates IS NOT NULL THEN
        -- Create a JSON object mapping the first date from event_dates to the event ID
        json_build_object(
            (SELECT value::text FROM json_array_elements(event_dates::json) LIMIT 1),
            google_calendar_event_id
        )
    WHEN google_calendar_event_id IS NOT NULL AND event_date IS NOT NULL THEN
        -- Fallback: use legacy event_date if event_dates is not set
        json_build_object(
            event_date::text,
            google_calendar_event_id
        )
    ELSE NULL
END
WHERE google_calendar_event_id IS NOT NULL;

