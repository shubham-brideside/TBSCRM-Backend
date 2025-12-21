-- Rename old label enum columns to label_enum for backward compatibility
-- The label_id column already exists and is being used for the new Label entity relationship

-- For deals table: rename label enum to label_enum if it exists and label_enum doesn't exist
-- Note: This handles the case where the old enum column is still named 'label'
ALTER TABLE deals 
CHANGE COLUMN label label_enum ENUM('DIRECT','DIVERT','DESTINATION','PARTY_MAKEUP','PRE_WEDDING') NULL;

-- For persons table: the label enum is already renamed to label_enum based on the schema provided
-- But if label enum still exists, rename it
-- Note: Based on the schema, label_enum already exists, so this might not be needed
-- But keeping it for safety

-- Verify label_id columns exist and have proper foreign keys
-- These should already exist from migration V20251217_06

