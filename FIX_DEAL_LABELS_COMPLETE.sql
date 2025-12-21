-- COMPLETE FIX: Clean up deal_labels and person_labels join tables
-- Join tables should ONLY have the foreign key columns

-- Step 1: Check current structure
DESCRIBE deal_labels;
DESCRIBE person_labels;

-- Step 2: Remove ALL unnecessary columns from deal_labels
-- Keep only deal_id and label_id
ALTER TABLE deal_labels DROP COLUMN IF EXISTS created_at;
ALTER TABLE deal_labels DROP COLUMN IF EXISTS is_deleted;
ALTER TABLE deal_labels DROP COLUMN IF EXISTS updated_at;
ALTER TABLE deal_labels DROP COLUMN IF EXISTS code;
ALTER TABLE deal_labels DROP COLUMN IF EXISTS name;
ALTER TABLE deal_labels DROP COLUMN IF EXISTS color;
ALTER TABLE deal_labels DROP COLUMN IF EXISTS display_name;
ALTER TABLE deal_labels DROP COLUMN IF EXISTS is_divert_label;
ALTER TABLE deal_labels DROP COLUMN IF EXISTS id;

-- Step 3: Remove ALL unnecessary columns from person_labels
-- Keep only person_id and label_id
ALTER TABLE person_labels DROP COLUMN IF EXISTS created_at;
ALTER TABLE person_labels DROP COLUMN IF EXISTS is_deleted;
ALTER TABLE person_labels DROP COLUMN IF EXISTS updated_at;
ALTER TABLE person_labels DROP COLUMN IF EXISTS code;
ALTER TABLE person_labels DROP COLUMN IF EXISTS name;
ALTER TABLE person_labels DROP COLUMN IF EXISTS color;
ALTER TABLE person_labels DROP COLUMN IF EXISTS display_name;
ALTER TABLE person_labels DROP COLUMN IF EXISTS is_divert_label;
ALTER TABLE person_labels DROP COLUMN IF EXISTS id;

-- Step 4: Verify - should only show deal_id and label_id (or person_id and label_id)
DESCRIBE deal_labels;
DESCRIBE person_labels;

