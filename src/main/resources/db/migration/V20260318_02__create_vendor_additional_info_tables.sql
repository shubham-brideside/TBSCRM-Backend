-- Vendor additional info: stores extra details per vendor (pricing, turnaround, travel, contract, logo).
CREATE TABLE vendor_additional_info (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    vendor_id BIGINT NOT NULL,
    starting_price_two_day_wedding VARCHAR(100) NULL,
    wedding_per_day VARCHAR(100) NULL,
    turnaround_time VARCHAR(200) NULL,
    photography_style VARCHAR(500) NULL,
    travel_accommodation_separate VARCHAR(10) NULL,
    vendor_contract_url VARCHAR(1024) NULL,
    vendor_logo_url VARCHAR(1024) NULL,
    created_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    updated_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
    CONSTRAINT fk_vendor_additional_info_vendor FOREIGN KEY (vendor_id)
        REFERENCES brideside_vendors(id) ON DELETE CASCADE,
    CONSTRAINT uq_vendor_additional_info_vendor UNIQUE (vendor_id)
);

CREATE INDEX idx_vendor_additional_info_vendor_id ON vendor_additional_info(vendor_id);

-- Custom fields for vendor additional info: dynamic label/type/value per vendor.
CREATE TABLE vendor_additional_custom_field (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    additional_info_id BIGINT NOT NULL,
    label VARCHAR(255) NOT NULL,
    field_type VARCHAR(20) NOT NULL,
    field_value VARCHAR(1000) NULL,
    created_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    updated_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
    CONSTRAINT fk_vendor_additional_custom_field_info FOREIGN KEY (additional_info_id)
        REFERENCES vendor_additional_info(id) ON DELETE CASCADE
);

CREATE INDEX idx_vendor_additional_custom_field_info_id ON vendor_additional_custom_field(additional_info_id);

