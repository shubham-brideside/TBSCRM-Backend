-- Add event_date_types column to store per-date event types for deals
-- This allows each event date to have its own eventType while keeping the legacy
-- event_type (single value) field for backward compatibility.

ALTER TABLE deals
ADD COLUMN IF NOT EXISTS event_date_types JSON NULL;


