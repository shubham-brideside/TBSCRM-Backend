-- Add created_by fields to deals table to track who created the deal (USER vs BOT)
ALTER TABLE deals 
ADD COLUMN created_by VARCHAR(10) DEFAULT 'USER',
ADD COLUMN created_by_user_id BIGINT NULL,
ADD COLUMN created_by_name VARCHAR(255) NULL;

-- Set default values for existing records
UPDATE deals 
SET created_by = 'USER' 
WHERE created_by IS NULL;

-- Add check constraint for created_by values
ALTER TABLE deals 
ADD CONSTRAINT chk_deals_created_by 
CHECK (created_by IN ('USER', 'BOT'));

-- Add foreign key constraint for created_by_user_id
ALTER TABLE deals 
ADD CONSTRAINT fk_deals_created_by_user 
FOREIGN KEY (created_by_user_id) REFERENCES users(id) ON DELETE SET NULL;

-- Add indexes for better query performance
CREATE INDEX idx_deals_created_by ON deals(created_by);
CREATE INDEX idx_deals_created_by_user_id ON deals(created_by_user_id);

