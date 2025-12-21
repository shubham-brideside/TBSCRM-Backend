-- FIX: Remove incorrect foreign key constraint on label_id
-- The deals table has TWO foreign keys on label_id - one correct, one wrong

-- Remove the INCORRECT foreign key that references deal_labels
ALTER TABLE deals 
DROP FOREIGN KEY FKepv24m8u9c8acm4n78ni9061k;

-- Verify the correct foreign key exists (should reference labels table)
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

-- The correct constraint FK3swa2v4cyvobscvp8a4d80brq should remain
-- It references labels(id) which is what we want

