-- Add email column to organizations
ALTER TABLE organizations
    ADD COLUMN IF NOT EXISTS email VARCHAR(255) NULL;
