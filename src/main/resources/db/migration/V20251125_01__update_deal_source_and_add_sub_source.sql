-- Update deal_source column to new enum values and add deal_sub_source column

-- Step 1: Add deal_sub_source column first (before updating data)
ALTER TABLE deals 
ADD COLUMN IF NOT EXISTS deal_sub_source VARCHAR(50) NULL;

-- Step 2: Drop existing constraint if it exists (to allow data updates)
ALTER TABLE deals 
DROP CONSTRAINT IF EXISTS chk_deal_source;

-- Step 3: Update existing deal_source values to new enum values
-- Map old values to new ones and set sub_source appropriately
UPDATE deals 
SET deal_source = 'DIRECT',
    deal_sub_source = CASE 
        WHEN deal_source = 'INSTAGRAM' THEN 'INSTAGRAM'
        WHEN deal_source = 'WHATSAPP' THEN 'WHATSAPP'
        WHEN deal_source = 'EMAIL' THEN 'EMAIL'
        WHEN deal_source = 'CALL' THEN NULL  -- Call doesn't map to new sub-sources
        WHEN deal_source = 'WEBSITE' THEN NULL  -- Website doesn't map to new sub-sources
        ELSE NULL
    END
WHERE deal_source IN ('INSTAGRAM', 'WHATSAPP', 'EMAIL', 'CALL', 'WEBSITE');

-- Note: REFERENCE already exists, so no update needed
-- DIVERT and PLANNER are new values, so no migration needed for them

-- Step 4: Add check constraint for deal_source values (new enum values)
ALTER TABLE deals 
ADD CONSTRAINT chk_deal_source 
CHECK (deal_source IS NULL OR deal_source IN ('DIRECT', 'DIVERT', 'REFERENCE', 'PLANNER'));

-- Step 5: Add check constraint for deal_sub_source values
ALTER TABLE deals 
DROP CONSTRAINT IF EXISTS chk_deal_sub_source;

ALTER TABLE deals 
ADD CONSTRAINT chk_deal_sub_source 
CHECK (deal_sub_source IS NULL OR deal_sub_source IN ('INSTAGRAM', 'WHATSAPP', 'LANDING_PAGE', 'EMAIL'));

-- Step 6: Add index for deal_sub_source if you plan to filter by it frequently
CREATE INDEX IF NOT EXISTS idx_deals_sub_source ON deals(deal_sub_source);

