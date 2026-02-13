-- Backfill lost_at for LOST deals (historical data before lost_at existed)
-- Uses updated_at when available, otherwise created_at
-- This will update all deals with status = 'LOST' that don't have lost_at set yet
UPDATE deals
SET lost_at = COALESCE(updated_at, created_at)
WHERE lost_at IS NULL
  AND status = 'LOST';
