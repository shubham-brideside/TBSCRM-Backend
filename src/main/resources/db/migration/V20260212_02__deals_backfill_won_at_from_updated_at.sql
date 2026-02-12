-- Backfill won_at from updated_at for existing WON deals (historical data before won_at existed)
UPDATE deals
SET won_at = updated_at
WHERE status = 'WON'
  AND won_at IS NULL
  AND updated_at IS NOT NULL;
