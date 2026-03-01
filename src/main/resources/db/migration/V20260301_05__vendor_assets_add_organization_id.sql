-- Add organization_id to vendor_assets
ALTER TABLE vendor_assets
    ADD COLUMN IF NOT EXISTS organization_id BIGINT NULL;

ALTER TABLE vendor_assets
    ADD CONSTRAINT fk_vendor_assets_organization
        FOREIGN KEY (organization_id) REFERENCES organizations(id) ON DELETE CASCADE;

CREATE INDEX IF NOT EXISTS idx_vendor_assets_organization_id ON vendor_assets (organization_id);
