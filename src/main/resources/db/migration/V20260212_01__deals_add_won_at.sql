-- Add won_at column to deals table for timestamp when deal was marked as won
ALTER TABLE deals
ADD COLUMN IF NOT EXISTS won_at DATETIME NULL;
