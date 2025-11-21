-- Add is_diverted column to deals table
ALTER TABLE deals 
ADD COLUMN IF NOT EXISTS is_diverted BIT(1) NOT NULL DEFAULT 0;

-- Add referenced_deal_id column to deals table (self-referencing FK to original deal)
ALTER TABLE deals 
ADD COLUMN IF NOT EXISTS referenced_deal_id BIGINT NULL;

-- Add foreign key constraint for referenced_deal_id
ALTER TABLE deals 
ADD CONSTRAINT FK_deals_referenced_deal 
FOREIGN KEY (referenced_deal_id) REFERENCES deals(id) ON DELETE SET NULL;

-- Add index for is_diverted for filtering
CREATE INDEX IF NOT EXISTS idx_deals_is_diverted ON deals(is_diverted);

-- Add index for referenced_deal_id for lookups
CREATE INDEX IF NOT EXISTS idx_deals_referenced_deal_id ON deals(referenced_deal_id);

