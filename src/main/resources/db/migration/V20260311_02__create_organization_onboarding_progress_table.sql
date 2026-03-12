-- Organization onboarding progress: tracks completion of each section on the Organization Details page.
-- Used to compute organization.is_active (all sections complete = is_active = true).
CREATE TABLE IF NOT EXISTS organization_onboarding_progress (
    organization_id BIGINT NOT NULL PRIMARY KEY,
    organization_details_complete TINYINT(1) NOT NULL DEFAULT 0,
    asset_info_complete TINYINT(1) NOT NULL DEFAULT 0,
    events_pricing_complete TINYINT(1) NOT NULL DEFAULT 0,
    vendor_data_complete TINYINT(1) NOT NULL DEFAULT 0,
    client_data_complete TINYINT(1) NOT NULL DEFAULT 0,
    team_members_complete TINYINT(1) NOT NULL DEFAULT 0,
    updated_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
    CONSTRAINT fk_org_progress_organization FOREIGN KEY (organization_id)
        REFERENCES organizations(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_org_progress_updated ON organization_onboarding_progress(updated_at);
