-- Add is_deleted column to persons table for soft delete
ALTER TABLE persons 
ADD COLUMN IF NOT EXISTS is_deleted BIT(1) NOT NULL DEFAULT 0;

-- Add index for is_deleted for filtering
CREATE INDEX IF NOT EXISTS idx_persons_is_deleted ON persons(is_deleted);

