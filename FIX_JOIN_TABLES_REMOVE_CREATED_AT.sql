-- RECOMMENDED FIX: Remove created_at from join tables
-- Join tables for many-to-many relationships don't need created_at columns
-- This is the cleanest solution

-- Remove created_at from deal_labels
ALTER TABLE deal_labels DROP COLUMN created_at;

-- Remove created_at from person_labels  
ALTER TABLE person_labels DROP COLUMN created_at;

-- Verify
DESCRIBE deal_labels;
DESCRIBE person_labels;

