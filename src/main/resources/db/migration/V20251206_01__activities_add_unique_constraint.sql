-- Migration: Add unique constraint to prevent duplicate activities
-- This prevents both frontend and backend from creating duplicate activities
-- when a deal moves to Qualified stage

BEGIN;

-- Add unique constraint on (deal_id, subject) to prevent duplicates
-- This ensures that only one activity with the same subject can exist per deal
ALTER TABLE activities 
ADD CONSTRAINT unique_deal_subject UNIQUE (deal_id, subject);

COMMIT;

