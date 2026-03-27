ALTER TABLE vendor_calendar_events
    ADD COLUMN IF NOT EXISTS team VARCHAR(255) NULL AFTER deal_id;
