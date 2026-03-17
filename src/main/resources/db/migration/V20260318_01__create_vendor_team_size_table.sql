-- Vendor team size table: stores per-guest-count and event-type resource details per vendor.
CREATE TABLE vendor_team_size (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    vendor_id BIGINT NOT NULL,
    guest_count VARCHAR(100) NOT NULL,
    event_type VARCHAR(100) NOT NULL,
    photographer VARCHAR(500) NULL,
    cinematographer VARCHAR(500) NULL,
    drone VARCHAR(500) NULL,
    notes VARCHAR(1000) NULL,
    created_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    updated_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
    CONSTRAINT fk_vendor_team_size_vendor FOREIGN KEY (vendor_id)
        REFERENCES brideside_vendors(id) ON DELETE CASCADE
);

CREATE INDEX idx_vendor_team_size_vendor_id ON vendor_team_size(vendor_id);

