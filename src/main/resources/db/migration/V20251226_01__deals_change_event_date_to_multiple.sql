-- Change event_date column from DATE to JSON to support multiple dates
-- First, add a new column for event dates as JSON
ALTER TABLE deals 
ADD COLUMN IF NOT EXISTS event_dates JSON NULL;

-- Migrate existing single event_date to event_dates JSON array
-- If event_date exists, convert it to a JSON array with one date
UPDATE deals 
SET event_dates = CASE 
    WHEN event_date IS NOT NULL THEN json_build_array(event_date::text)
    ELSE NULL
END
WHERE event_date IS NOT NULL;

-- Drop the old event_date column after migration
-- Note: We keep it for now to ensure backward compatibility during transition
-- ALTER TABLE deals DROP COLUMN IF EXISTS event_date;

