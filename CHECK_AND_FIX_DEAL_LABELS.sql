-- Step 1: Check the actual structure of deal_labels table
SHOW CREATE TABLE deal_labels;

-- Step 2: Check all columns in deal_labels
DESCRIBE deal_labels;

-- Step 3: If created_at exists, remove it
ALTER TABLE deal_labels DROP COLUMN created_at;

-- Step 4: Verify - should only show deal_id and label_id
DESCRIBE deal_labels;

