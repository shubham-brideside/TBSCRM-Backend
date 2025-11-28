-- Add is_deleted column to deals table for soft delete
ALTER TABLE deals 
ADD COLUMN IF NOT EXISTS is_deleted BIT(1) NOT NULL DEFAULT 0;

-- Add index for is_deleted for filtering
CREATE INDEX IF NOT EXISTS idx_deals_is_deleted ON deals(is_deleted);

