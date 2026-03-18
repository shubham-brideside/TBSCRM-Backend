-- Backfill: recompute organization_activation.activated and organizations.is_active
-- using only the required 4 activation checklist flags (contract signed, onboarding fee,
-- gmail+login, phone+sim) instead of the old "17-item completed_count" logic.
--
-- We handle both possible column names for vendor contract:
-- - vendor_contract
-- - contract_signed

SET @has_vendor_contract :=
  (SELECT COUNT(*)
   FROM information_schema.COLUMNS
   WHERE TABLE_SCHEMA = DATABASE()
     AND TABLE_NAME = 'organization_activation'
     AND COLUMN_NAME = 'vendor_contract');

SET @has_contract_signed :=
  (SELECT COUNT(*)
   FROM information_schema.COLUMNS
   WHERE TABLE_SCHEMA = DATABASE()
     AND TABLE_NAME = 'organization_activation'
     AND COLUMN_NAME = 'contract_signed');

SET @sql :=
  CASE
    WHEN @has_vendor_contract > 0 AND @has_contract_signed > 0 THEN
      'UPDATE organization_activation
       SET activated = IF(
         ((vendor_contract = 1 OR contract_signed = 1)
           AND onboarding_fee_received = 1
           AND gmail_and_login_credentials = 1
           AND phone_and_sim_issued = 1),
         1, 0)'
    WHEN @has_vendor_contract > 0 THEN
      'UPDATE organization_activation
       SET activated = IF(
         ((vendor_contract = 1)
           AND onboarding_fee_received = 1
           AND gmail_and_login_credentials = 1
           AND phone_and_sim_issued = 1),
         1, 0)'
    WHEN @has_contract_signed > 0 THEN
      'UPDATE organization_activation
       SET activated = IF(
         ((contract_signed = 1)
           AND onboarding_fee_received = 1
           AND gmail_and_login_credentials = 1
           AND phone_and_sim_issued = 1),
         1, 0)'
    ELSE
      'SELECT 1'
  END;

PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

UPDATE organizations o
INNER JOIN organization_activation a ON a.organization_id = o.id
SET o.is_active = IF(a.activated = 1, 1, 0);

