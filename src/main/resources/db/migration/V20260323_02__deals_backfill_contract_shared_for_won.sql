-- Ensure all won deals have contract_shared set to true
UPDATE deals
SET contract_shared = TRUE
WHERE status = 'WON'
   OR won = TRUE;
