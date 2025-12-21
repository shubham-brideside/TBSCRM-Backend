-- Fix labels table: ensure code column is nullable with default empty string
-- This handles the case where the table was created with a NOT NULL code column

-- First, update any existing NULL values to empty string (if any)
UPDATE labels SET code = '' WHERE code IS NULL;

-- Modify column to allow NULL and set default to empty string
ALTER TABLE labels 
MODIFY COLUMN code VARCHAR(50) NULL DEFAULT '';

