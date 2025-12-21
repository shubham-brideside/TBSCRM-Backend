-- WORKING FIX: Clean up deal_labels join table
-- Run each command one by one - ignore errors if column doesn't exist

-- First, check what columns exist
DESCRIBE deal_labels;

-- Remove unnecessary columns (run each - if it says column doesn't exist, skip it)
ALTER TABLE deal_labels DROP COLUMN created_at;
ALTER TABLE deal_labels DROP COLUMN is_deleted;
ALTER TABLE deal_labels DROP COLUMN updated_at;
ALTER TABLE deal_labels DROP COLUMN code;
ALTER TABLE deal_labels DROP COLUMN name;
ALTER TABLE deal_labels DROP COLUMN color;
ALTER TABLE deal_labels DROP COLUMN display_name;
ALTER TABLE deal_labels DROP COLUMN is_divert_label;
ALTER TABLE deal_labels DROP COLUMN id;

-- Verify - should only show deal_id and label_id
DESCRIBE deal_labels;

-- Do the same for person_labels
DESCRIBE person_labels;

ALTER TABLE person_labels DROP COLUMN created_at;
ALTER TABLE person_labels DROP COLUMN is_deleted;
ALTER TABLE person_labels DROP COLUMN updated_at;
ALTER TABLE person_labels DROP COLUMN code;
ALTER TABLE person_labels DROP COLUMN name;
ALTER TABLE person_labels DROP COLUMN color;
ALTER TABLE person_labels DROP COLUMN display_name;
ALTER TABLE person_labels DROP COLUMN is_divert_label;
ALTER TABLE person_labels DROP COLUMN id;

-- Verify person_labels
DESCRIBE person_labels;

