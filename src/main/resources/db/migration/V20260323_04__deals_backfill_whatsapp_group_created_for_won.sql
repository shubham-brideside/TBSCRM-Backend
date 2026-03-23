-- Ensure all won deals have whatsapp_group_created set to true
UPDATE deals
SET whatsapp_group_created = TRUE
WHERE status = 'WON'
   OR won = TRUE;
