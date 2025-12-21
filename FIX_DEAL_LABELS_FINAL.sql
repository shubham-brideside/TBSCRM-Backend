-- FINAL FIX for deal_labels and person_labels tables
-- Run these commands in order on your MySQL database

-- ============================================
-- FIX 1: deal_labels table
-- ============================================

-- Check if created_at column exists and what its current definition is
SELECT COLUMN_NAME, COLUMN_TYPE, IS_NULLABLE, COLUMN_DEFAULT 
FROM information_schema.COLUMNS 
WHERE TABLE_SCHEMA = DATABASE() 
AND TABLE_NAME = 'deal_labels' 
AND COLUMN_NAME = 'created_at';

-- Option A: If column exists but doesn't have default, modify it
ALTER TABLE deal_labels 
MODIFY COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Option B: If column doesn't exist, add it (run this if Option A fails)
-- ALTER TABLE deal_labels 
-- ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- ============================================
-- FIX 2: person_labels table  
-- ============================================

-- Check if created_at column exists and what its current definition is
SELECT COLUMN_NAME, COLUMN_TYPE, IS_NULLABLE, COLUMN_DEFAULT 
FROM information_schema.COLUMNS 
WHERE TABLE_SCHEMA = DATABASE() 
AND TABLE_NAME = 'person_labels' 
AND COLUMN_NAME = 'created_at';

-- Option A: If column exists but doesn't have default, modify it
ALTER TABLE person_labels 
MODIFY COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Option B: If column doesn't exist, add it (run this if Option A fails)
-- ALTER TABLE person_labels 
-- ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- ============================================
-- VERIFY
-- ============================================
DESCRIBE deal_labels;
DESCRIBE person_labels;

