-- IMMEDIATE FIX: Run these SQL commands directly on your database
-- This will fix the created_at columns in deal_labels and person_labels join tables

-- Fix deal_labels table
-- Step 1: Try to add created_at column (will fail if already exists - that's okay)
-- If you get "Duplicate column name" error, skip to Step 2
ALTER TABLE deal_labels 
ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Step 2: Ensure created_at has default value (run this even if Step 1 failed)
ALTER TABLE deal_labels 
MODIFY COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Fix person_labels table
-- Step 3: Try to add created_at column (will fail if already exists - that's okay)
-- If you get "Duplicate column name" error, skip to Step 4
ALTER TABLE person_labels 
ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Step 4: Ensure created_at has default value (run this even if Step 3 failed)
ALTER TABLE person_labels 
MODIFY COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Verify the changes
DESCRIBE deal_labels;
DESCRIBE person_labels;

