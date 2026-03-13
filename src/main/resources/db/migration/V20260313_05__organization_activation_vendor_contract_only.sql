-- One column only: vendor_contract. Drop contract_signed after merging values.

SET @cs := (SELECT COUNT(*) FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organization_activation' AND COLUMN_NAME = 'contract_signed');
SET @vc := (SELECT COUNT(*) FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organization_activation' AND COLUMN_NAME = 'vendor_contract');

-- Both columns: merge into vendor_contract, then drop contract_signed
SET @sql := IF(@cs > 0 AND @vc > 0,
    'UPDATE organization_activation SET vendor_contract = IF(contract_signed = 1 OR vendor_contract = 1, 1, 0)',
    'SELECT 1');
PREPARE s1 FROM @sql; EXECUTE s1; DEALLOCATE PREPARE s1;

SET @sql := IF(@cs > 0 AND @vc > 0,
    'ALTER TABLE organization_activation DROP COLUMN contract_signed',
    'SELECT 1');
PREPARE s2 FROM @sql; EXECUTE s2; DEALLOCATE PREPARE s2;

-- Only contract_signed: rename to vendor_contract
SET @cs := (SELECT COUNT(*) FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organization_activation' AND COLUMN_NAME = 'contract_signed');
SET @vc := (SELECT COUNT(*) FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organization_activation' AND COLUMN_NAME = 'vendor_contract');

SET @sql := IF(@cs > 0 AND @vc = 0,
    'ALTER TABLE organization_activation CHANGE COLUMN contract_signed vendor_contract TINYINT(1) NOT NULL DEFAULT 0',
    'SELECT 1');
PREPARE s3 FROM @sql; EXECUTE s3; DEALLOCATE PREPARE s3;

-- If vendor_contract missing entirely (old create only had contract_signed and rename failed): add column — handled by s3
