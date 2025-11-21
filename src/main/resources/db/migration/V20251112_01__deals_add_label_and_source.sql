-- Add label column to deals table
ALTER TABLE deals 
ADD COLUMN IF NOT EXISTS label VARCHAR(50) NULL;

-- Add deal_source column to deals table (note: source_id already exists for Source entity relationship)
ALTER TABLE deals 
ADD COLUMN IF NOT EXISTS deal_source VARCHAR(50) NULL;

-- Add check constraint for label values (enum values use underscores)
ALTER TABLE deals 
ADD CONSTRAINT chk_deal_label 
CHECK (label IS NULL OR label IN ('DIRECT', 'DIVERT', 'DESTINATION', 'PARTY_MAKEUP', 'PRE_WEDDING'));

-- Add check constraint for deal_source values
ALTER TABLE deals 
ADD CONSTRAINT chk_deal_source 
CHECK (deal_source IS NULL OR deal_source IN ('INSTAGRAM', 'WHATSAPP', 'EMAIL', 'REFERENCE', 'CALL', 'WEBSITE'));

-- Add index for label if you plan to filter by it frequently
CREATE INDEX IF NOT EXISTS idx_deals_label ON deals(label);

-- Add index for deal_source if you plan to filter by it frequently
CREATE INDEX IF NOT EXISTS idx_deals_source ON deals(deal_source);

