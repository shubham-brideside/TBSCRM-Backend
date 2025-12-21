-- Fix join tables: add created_at column with default value if it doesn't exist
-- This handles the case where tables were created with created_at but without default

-- Fix deal_labels table
DELIMITER $$

CREATE PROCEDURE FixDealLabelsCreatedAt()
BEGIN
    DECLARE column_exists INT DEFAULT 0;
    
    SELECT COUNT(*) INTO column_exists
    FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'deal_labels'
    AND COLUMN_NAME = 'created_at';
    
    IF column_exists = 0 THEN
        ALTER TABLE deal_labels ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;
    ELSE
        -- Column exists, ensure it has default value
        ALTER TABLE deal_labels MODIFY COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;
    END IF;
END$$

CREATE PROCEDURE FixPersonLabelsCreatedAt()
BEGIN
    DECLARE column_exists INT DEFAULT 0;
    
    SELECT COUNT(*) INTO column_exists
    FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'person_labels'
    AND COLUMN_NAME = 'created_at';
    
    IF column_exists = 0 THEN
        ALTER TABLE person_labels ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;
    ELSE
        -- Column exists, ensure it has default value
        ALTER TABLE person_labels MODIFY COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;
    END IF;
END$$

DELIMITER ;

CALL FixDealLabelsCreatedAt();
DROP PROCEDURE FixDealLabelsCreatedAt;

CALL FixPersonLabelsCreatedAt();
DROP PROCEDURE FixPersonLabelsCreatedAt;

