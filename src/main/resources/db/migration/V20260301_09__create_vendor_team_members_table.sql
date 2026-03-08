-- Vendor team members table: stores team member details (name, designation, instagram) per vendor.
CREATE TABLE vendor_team_members (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    vendor_id BIGINT NOT NULL,
    name VARCHAR(255) NOT NULL,
    designation VARCHAR(255) NULL,
    instagram_id VARCHAR(255) NULL,
    created_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    updated_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
    CONSTRAINT fk_vendor_team_members_vendor FOREIGN KEY (vendor_id)
        REFERENCES brideside_vendors(id) ON DELETE CASCADE
);

CREATE INDEX idx_vendor_team_members_vendor_id ON vendor_team_members(vendor_id);
