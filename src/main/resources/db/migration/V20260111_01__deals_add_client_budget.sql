-- Add client_budget column to deals table for tracking client budget when deal is lost due to budget reason
ALTER TABLE deals 
ADD COLUMN IF NOT EXISTS client_budget DECIMAL(15, 2) NULL;

