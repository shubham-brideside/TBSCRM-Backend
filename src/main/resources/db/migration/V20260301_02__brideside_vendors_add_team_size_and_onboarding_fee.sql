-- Add vendor team size & onboarding fee fields
ALTER TABLE brideside_vendors
    ADD COLUMN IF NOT EXISTS team_size INT NULL,
    ADD COLUMN IF NOT EXISTS onboarding_fee DECIMAL(12,2) NULL;

