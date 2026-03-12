-- Add is_active column to organizations. Default FALSE - organization becomes active
-- only when all onboarding details are complete (Organization details, Asset Info,
-- Events Pricing, Vendor Data, Client Data, Team Members).
ALTER TABLE organizations
    ADD COLUMN IF NOT EXISTS is_active TINYINT(1) NOT NULL DEFAULT 0;

CREATE INDEX IF NOT EXISTS idx_organizations_is_active ON organizations(is_active);
