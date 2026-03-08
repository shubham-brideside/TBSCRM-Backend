-- Add about column to store details of vendor services
ALTER TABLE brideside_vendors
    ADD COLUMN IF NOT EXISTS about TEXT NULL;
