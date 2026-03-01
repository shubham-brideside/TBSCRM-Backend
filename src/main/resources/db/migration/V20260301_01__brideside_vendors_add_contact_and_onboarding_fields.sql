-- Add vendor contact & onboarding fields
ALTER TABLE brideside_vendors
    ADD COLUMN IF NOT EXISTS contact_number VARCHAR(50) NULL,
    ADD COLUMN IF NOT EXISTS office_studio_location VARCHAR(255) NULL,
    ADD COLUMN IF NOT EXISTS base_location VARCHAR(255) NULL,
    ADD COLUMN IF NOT EXISTS official_number VARCHAR(50) NULL,
    ADD COLUMN IF NOT EXISTS email_id VARCHAR(255) NULL,
    ADD COLUMN IF NOT EXISTS onboarding_date DATETIME(6) NULL;

