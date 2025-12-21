-- Remove created_at columns from join tables
-- Join tables for many-to-many relationships don't need created_at columns
-- This migration removes them if they exist

DELIMITER $$

CREATE PROCEDURE RemoveCreatedAtFromDealLabels()
BEGIN
    DECLARE column_exists INT DEFAULT 0;
    
    SELECT COUNT(*) INTO column_exists
    FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'deal_labels'
    AND COLUMN_NAME = 'created_at';
    
    IF column_exists > 0 THEN
        ALTER TABLE deal_labels DROP COLUMN created_at;
    END IF;
END$$

CREATE PROCEDURE RemoveCreatedAtFromPersonLabels()
BEGIN
    DECLARE column_exists INT DEFAULT 0;
    
    SELECT COUNT(*) INTO column_exists
    FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'person_labels'
    AND COLUMN_NAME = 'created_at';
    
    IF column_exists > 0 THEN
        ALTER TABLE person_labels DROP COLUMN created_at;
    END IF;
END$$

DELIMITER ;

CALL RemoveCreatedAtFromDealLabels();
DROP PROCEDURE RemoveCreatedAtFromDealLabels;

CALL RemoveCreatedAtFromPersonLabels();
DROP PROCEDURE RemoveCreatedAtFromPersonLabels;

