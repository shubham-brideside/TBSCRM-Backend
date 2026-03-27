ALTER TABLE deal_calendar_event_types
    ADD COLUMN IF NOT EXISTS team VARCHAR(255) NULL AFTER event_type;
