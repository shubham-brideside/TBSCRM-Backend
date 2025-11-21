ALTER TABLE organizations
    ADD COLUMN google_calendar_id VARCHAR(255) NULL AFTER address;

ALTER TABLE deals
    ADD COLUMN google_calendar_event_id VARCHAR(255) NULL AFTER event_date;



