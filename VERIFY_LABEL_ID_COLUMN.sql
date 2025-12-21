-- Verify label_id column exists in deals table
DESCRIBE deals;

-- Check if label_id column exists and has foreign key
SELECT 
    COLUMN_NAME,
    COLUMN_TYPE,
    IS_NULLABLE,
    COLUMN_KEY
FROM information_schema.COLUMNS
WHERE TABLE_SCHEMA = DATABASE()
AND TABLE_NAME = 'deals'
AND COLUMN_NAME = 'label_id';

-- Check foreign key constraint
SELECT 
    CONSTRAINT_NAME,
    TABLE_NAME,
    COLUMN_NAME,
    REFERENCED_TABLE_NAME,
    REFERENCED_COLUMN_NAME
FROM information_schema.KEY_COLUMN_USAGE
WHERE TABLE_SCHEMA = DATABASE()
AND TABLE_NAME = 'deals'
AND COLUMN_NAME = 'label_id';

-- Verify label 5 exists
SELECT * FROM labels WHERE id = 5;

-- Test: Try to manually set label_id (this will show the actual error)
-- UPDATE deals SET label_id = 5 WHERE id = 1 LIMIT 1;

