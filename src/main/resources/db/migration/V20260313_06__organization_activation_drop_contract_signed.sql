-- Fix: INSERT only fills vendor_contract (JPA). MySQL error if contract_signed still exists NOT NULL no default.
-- 1) Default so any legacy INSERTs don't break (harmless if we drop next)
-- 2) Merge + drop duplicate column so only vendor_contract remains

SET @cs := (SELECT COUNT(*) FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organization_activation' AND COLUMN_NAME = 'contract_signed');
SET @vc := (SELECT COUNT(*) FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organization_activation' AND COLUMN_NAME = 'vendor_contract');

-- Allow INSERT without contract_signed (stops 500 until drop runs)
SET @sql := IF(@cs > 0,
    'ALTER TABLE organization_activation MODIFY COLUMN contract_signed TINYINT(1) NOT NULL DEFAULT 0',
    'SELECT 1');
PREPARE x1 FROM @sql; EXECUTE x1; DEALLOCATE PREPARE x1;

-- Both columns: keep vendor_contract truth, then drop contract_signed
SET @sql := IF(@cs > 0 AND @vc > 0,
    'UPDATE organization_activation SET vendor_contract = IF(contract_signed = 1 OR vendor_contract = 1, 1, 0)',
    'SELECT 1');
PREPARE x2 FROM @sql; EXECUTE x2; DEALLOCATE PREPARE x2;

SET @sql := IF(@cs > 0 AND @vc > 0,
    'ALTER TABLE organization_activation DROP COLUMN contract_signed',
    'SELECT 1');
PREPARE x3 FROM @sql; EXECUTE x3; DEALLOCATE PREPARE x3;

-- Only contract_signed (no vendor_contract): rename so JPA matches
SET @cs := (SELECT COUNT(*) FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organization_activation' AND COLUMN_NAME = 'contract_signed');
SET @vc := (SELECT COUNT(*) FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organization_activation' AND COLUMN_NAME = 'vendor_contract');

SET @sql := IF(@cs > 0 AND @vc = 0,
    'ALTER TABLE organization_activation CHANGE COLUMN contract_signed vendor_contract TINYINT(1) NOT NULL DEFAULT 0',
    'SELECT 1');
PREPARE x4 FROM @sql; EXECUTE x4; DEALLOCATE PREPARE x4;
