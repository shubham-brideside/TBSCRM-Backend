CREATE TABLE vendor_calendar_events (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    organization_id BIGINT NOT NULL,
    google_calendar_id VARCHAR(255) NOT NULL,
    google_event_id VARCHAR(255) NOT NULL,
    summary VARCHAR(500),
    description TEXT,
    start_at TIMESTAMP NOT NULL,
    end_at TIMESTAMP NOT NULL,
    all_day BOOLEAN NOT NULL DEFAULT FALSE,
    status VARCHAR(50),
    last_synced_at TIMESTAMP NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    CONSTRAINT fk_vendor_calendar_events_org FOREIGN KEY (organization_id) REFERENCES organizations(id),
    CONSTRAINT uq_vendor_calendar_event UNIQUE (google_event_id)
);

CREATE INDEX idx_vendor_calendar_events_org ON vendor_calendar_events (organization_id);
CREATE INDEX idx_vendor_calendar_events_dates ON vendor_calendar_events (start_at, end_at);




