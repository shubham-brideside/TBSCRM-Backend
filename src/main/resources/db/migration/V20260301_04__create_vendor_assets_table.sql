-- Create vendor_assets table for storing vendor asset info (phone, SIM, etc.)
CREATE TABLE IF NOT EXISTS vendor_assets (
    id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    vendor_id BIGINT NOT NULL,
    phone_model VARCHAR(255) NULL,
    phone_issued_by VARCHAR(255) NULL,
    sim_card VARCHAR(255) NULL,
    sim_issued_by VARCHAR(255) NULL,
    issued_on DATE NULL,
    created_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    updated_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
    CONSTRAINT fk_vendor_assets_vendor FOREIGN KEY (vendor_id) REFERENCES brideside_vendors(id) ON DELETE CASCADE
);

CREATE INDEX idx_vendor_assets_vendor_id ON vendor_assets (vendor_id);
