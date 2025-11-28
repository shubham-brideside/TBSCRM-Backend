-- Add lost_reason column to deals table for tracking why a deal was marked as lost
ALTER TABLE deals 
ADD COLUMN IF NOT EXISTS lost_reason VARCHAR(50) NULL;

-- Add check constraint for lost_reason values (enum values use underscores)
ALTER TABLE deals 
ADD CONSTRAINT chk_deal_lost_reason 
CHECK (lost_reason IS NULL OR lost_reason IN (
    'SLOT_NOT_OPENED',
    'NOT_INTERESTED',
    'DATE_POSTPONED',
    'NOT_AVAILABLE',
    'GHOSTED',
    'BUDGET',
    'BOOKED_SOMEONE_ELSE'
));

-- Add index for lost_reason if you plan to filter by it frequently
CREATE INDEX IF NOT EXISTS idx_deals_lost_reason ON deals(lost_reason);

