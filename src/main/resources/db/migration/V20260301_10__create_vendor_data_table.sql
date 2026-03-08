-- Vendor data table: stores URLs (master data link, calendar sheet link) per vendor.
-- One row per vendor.
CREATE TABLE vendor_data (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    vendor_id BIGINT NOT NULL,
    master_data_link TEXT NULL,
    calendar_sheet_link TEXT NULL,
    created_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    updated_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
    CONSTRAINT fk_vendor_data_vendor FOREIGN KEY (vendor_id) REFERENCES brideside_vendors(id) ON DELETE CASCADE,
    CONSTRAINT uq_vendor_data_vendor UNIQUE (vendor_id)
);

CREATE INDEX idx_vendor_data_vendor_id ON vendor_data(vendor_id);
