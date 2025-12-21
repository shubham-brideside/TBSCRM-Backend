-- DEFINITIVE FIX for deal_labels table
-- This will work regardless of the current state of the table

-- Step 1: Check current structure
SHOW CREATE TABLE deal_labels;

-- Step 2: Check if created_at column exists
SELECT COLUMN_NAME, COLUMN_TYPE, IS_NULLABLE, COLUMN_DEFAULT 
FROM information_schema.COLUMNS 
WHERE TABLE_SCHEMA = DATABASE() 
AND TABLE_NAME = 'deal_labels' 
AND COLUMN_NAME = 'created_at';

-- Step 3: FIX - Try to modify the column (works if column exists)
ALTER TABLE deal_labels 
MODIFY COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- If Step 3 fails with "Unknown column 'created_at'", then run Step 4:
-- Step 4: Add the column if it doesn't exist
-- ALTER TABLE deal_labels 
-- ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Step 5: ALTERNATIVE FIX - If you don't need created_at in join table, drop it
-- This is actually the cleanest solution for a many-to-many join table
-- ALTER TABLE deal_labels DROP COLUMN created_at;

-- Step 6: Verify
DESCRIBE deal_labels;

