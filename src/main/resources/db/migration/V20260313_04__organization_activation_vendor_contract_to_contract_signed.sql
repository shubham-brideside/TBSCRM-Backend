-- Vendor contract on UI == contract_signed in DB (one column). If V03 renamed to vendor_contract, rename back.
SET @col := (
    SELECT COLUMN_NAME FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE()
      AND TABLE_NAME = 'organization_activation'
      AND COLUMN_NAME = 'vendor_contract'
    LIMIT 1
);
SET @sql := IF(
    @col IS NOT NULL,
    'ALTER TABLE organization_activation CHANGE COLUMN vendor_contract contract_signed TINYINT(1) NOT NULL DEFAULT 0',
    'SELECT 1'
);
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;
