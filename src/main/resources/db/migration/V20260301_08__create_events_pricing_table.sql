-- Events pricing table: stores pricing per event/resource per vendor, session, and artist level.
-- Photography: session='DEFAULT', artist_level='PRIMARY' only.
-- Makeup: session='CURRENT' or 'UPCOMING', artist_level='PRIMARY', 'SENIOR', 'JUNIOR'.
CREATE TABLE events_pricing (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    vendor_id BIGINT NOT NULL,
    session VARCHAR(20) NOT NULL,
    artist_level VARCHAR(20) NOT NULL,
    event_code VARCHAR(100) NOT NULL,
    event_label VARCHAR(255) NULL,
    base_price DECIMAL(12, 2) NULL,
    destination_price DECIMAL(12, 2) NULL,
    additional_makeup_price DECIMAL(12, 2) NULL,
    availability_at_studio TEXT NULL,
    policy_notes TEXT NULL,
    currency VARCHAR(10) NULL,
    display_order INT NULL,
    created_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    updated_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
    CONSTRAINT fk_events_pricing_vendor FOREIGN KEY (vendor_id)
        REFERENCES brideside_vendors(id) ON DELETE CASCADE,
    CONSTRAINT uq_events_pricing_vendor_session_level_code
        UNIQUE (vendor_id, session, artist_level, event_code)
);

CREATE INDEX idx_events_pricing_vendor_session ON events_pricing(vendor_id, session);
