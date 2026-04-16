-- Manual diversions: created_by should be USER (diverting user lives on diverted_by_* only).
UPDATE deals
SET created_by = 'USER'
WHERE diverted_by_user_id IS NOT NULL
  AND created_by = 'BOT';
