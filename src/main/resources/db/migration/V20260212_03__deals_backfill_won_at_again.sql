-- Backfill won_at for WON deals (retry with broader conditions).
-- Covers: status = 'WON' or legacy won = 1; uses updated_at or created_at when updated_at is null.
UPDATE deals
SET won_at = COALESCE(updated_at, created_at)
WHERE won_at IS NULL
  AND (status = 'WON' OR won = 1);
