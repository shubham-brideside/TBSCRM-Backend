-- Add lost_at column to deals table for timestamp when deal was marked as lost
ALTER TABLE deals
ADD COLUMN IF NOT EXISTS lost_at DATETIME NULL;
