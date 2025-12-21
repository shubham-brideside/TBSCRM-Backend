-- Add is_deleted column to labels table if it doesn't exist
-- Note: This will fail if column already exists, which is fine - just means it's already there

-- Check if column exists and add if not (MySQL doesn't support IF NOT EXISTS for ALTER TABLE)
-- Using a stored procedure approach
DELIMITER $$

CREATE PROCEDURE AddIsDeletedColumnIfNotExists()
BEGIN
    DECLARE column_exists INT DEFAULT 0;
    
    SELECT COUNT(*) INTO column_exists
    FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'labels'
    AND COLUMN_NAME = 'is_deleted';
    
    IF column_exists = 0 THEN
        ALTER TABLE labels ADD COLUMN is_deleted BOOLEAN NOT NULL DEFAULT FALSE;
    END IF;
END$$

DELIMITER ;

CALL AddIsDeletedColumnIfNotExists();
DROP PROCEDURE AddIsDeletedColumnIfNotExists;

-- Ensure default value is set
ALTER TABLE labels MODIFY COLUMN is_deleted BOOLEAN NOT NULL DEFAULT FALSE;

