-- Alternative Migration script - Use this if the column is VARCHAR (not ENUM)
-- This script is simpler and works if the name column is VARCHAR

-- Update roles table: Change MANAGER to SALES
UPDATE roles 
SET name = 'SALES' 
WHERE name = 'MANAGER';

-- Update roles table: Change SALESREP to PRESALES
UPDATE roles 
SET name = 'PRESALES' 
WHERE name = 'SALESREP';

-- Verify the updates
SELECT id, name, description FROM roles ORDER BY id;

