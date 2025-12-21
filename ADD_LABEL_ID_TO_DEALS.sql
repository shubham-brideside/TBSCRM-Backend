-- IMMEDIATE FIX: Add label_id column to deals table if it doesn't exist
-- Run this SQL directly on your database

-- Check if column exists first
SELECT COLUMN_NAME 
FROM information_schema.COLUMNS 
WHERE TABLE_SCHEMA = DATABASE() 
AND TABLE_NAME = 'deals' 
AND COLUMN_NAME = 'label_id';

-- Add label_id column if it doesn't exist
ALTER TABLE deals 
ADD COLUMN IF NOT EXISTS label_id BIGINT NULL;

-- Add foreign key constraint if it doesn't exist
-- First, check if foreign key already exists
SELECT CONSTRAINT_NAME 
FROM information_schema.KEY_COLUMN_USAGE 
WHERE TABLE_SCHEMA = DATABASE() 
AND TABLE_NAME = 'deals' 
AND COLUMN_NAME = 'label_id' 
AND REFERENCED_TABLE_NAME = 'labels';

-- Add foreign key (will fail if already exists - that's okay)
ALTER TABLE deals 
ADD CONSTRAINT fk_deals_label_id 
FOREIGN KEY (label_id) REFERENCES labels(id) ON DELETE SET NULL;

-- Create index for better performance
CREATE INDEX IF NOT EXISTS idx_deals_label_id ON deals(label_id);

-- Verify
DESCRIBE deals;

