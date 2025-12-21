-- IMMEDIATE FIX for deal_labels table
-- Run this SQL command directly on your database

-- Check current structure
DESCRIBE deal_labels;

-- Fix: Add or modify created_at column with default value
-- This will work whether the column exists or not

-- If column doesn't exist, add it
ALTER TABLE deal_labels 
ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- If column exists but doesn't have default, modify it
ALTER TABLE deal_labels 
MODIFY COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Verify
DESCRIBE deal_labels;

