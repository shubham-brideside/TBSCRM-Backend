-- IMMEDIATE FIX: Run these SQL commands directly on your database
-- This will fix the code and is_deleted columns

-- Step 1: Update any existing NULL code values (if any)
UPDATE labels SET code = '' WHERE code IS NULL;

-- Step 2: Make the code column nullable with a default empty string
ALTER TABLE labels MODIFY COLUMN code VARCHAR(50) NULL DEFAULT '';

-- Step 3: Add is_deleted column (will fail if already exists - that's okay, skip to step 4)
-- If this fails with "Duplicate column name", the column already exists, proceed to step 4
ALTER TABLE labels ADD COLUMN is_deleted BOOLEAN NOT NULL DEFAULT FALSE;

-- Step 4: Ensure is_deleted has default value (run this even if step 3 failed)
UPDATE labels SET is_deleted = FALSE WHERE is_deleted IS NULL;
ALTER TABLE labels MODIFY COLUMN is_deleted BOOLEAN NOT NULL DEFAULT FALSE;

-- Step 5: Fix join tables created_at columns
-- Fix deal_labels table - add column if it doesn't exist (will fail if exists, that's okay)
-- If this fails with "Duplicate column name", the column already exists, proceed to next ALTER
ALTER TABLE deal_labels 
ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Ensure created_at has default value (run this even if above failed)
ALTER TABLE deal_labels 
MODIFY COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Fix person_labels table - add column if it doesn't exist (will fail if exists, that's okay)
-- If this fails with "Duplicate column name", the column already exists, proceed to next ALTER
ALTER TABLE person_labels 
ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Ensure created_at has default value (run this even if above failed)
ALTER TABLE person_labels 
MODIFY COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Step 6: Verify the changes
DESCRIBE labels;
DESCRIBE deal_labels;
DESCRIBE person_labels;

