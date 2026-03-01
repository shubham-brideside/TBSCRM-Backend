-- Add vendor name column
ALTER TABLE brideside_vendors
    ADD COLUMN IF NOT EXISTS vendor_name VARCHAR(255) NULL;
