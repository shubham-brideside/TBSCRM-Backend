ALTER TABLE vendor_calendar_events
    ADD COLUMN IF NOT EXISTS deal_id BIGINT NULL AFTER description;

CREATE INDEX idx_vendor_calendar_events_deal_id ON vendor_calendar_events (deal_id);
