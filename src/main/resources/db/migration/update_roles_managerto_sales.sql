-- Migration script to update role names from MANAGER to SALES and SALESREP to PRESALES
-- Run this script on your database to update existing role records

-- Step 1: Check if the column is an ENUM type and modify it
-- If the name column is an ENUM, we need to modify it to include the new values
-- Note: MySQL doesn't allow direct modification of ENUM values, so we'll convert to VARCHAR first if needed

-- Option 1: If column is ENUM, modify the ENUM definition
-- First, update the ENUM to include new values (MySQL requires this)
ALTER TABLE roles 
MODIFY COLUMN name ENUM('ADMIN', 'CATEGORY_MANAGER', 'SALES', 'PRESALES', 'MANAGER', 'SALESREP') 
NOT NULL;

-- Step 2: Update the data
UPDATE roles 
SET name = 'SALES' 
WHERE name = 'MANAGER';

UPDATE roles 
SET name = 'PRESALES' 
WHERE name = 'SALESREP';

-- Step 3: Remove old ENUM values (optional, but recommended for clean schema)
ALTER TABLE roles 
MODIFY COLUMN name ENUM('ADMIN', 'CATEGORY_MANAGER', 'SALES', 'PRESALES') 
NOT NULL;

-- Verify the updates
SELECT id, name, description FROM roles ORDER BY id;

