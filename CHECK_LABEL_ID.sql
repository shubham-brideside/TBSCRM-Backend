-- Check if label ID 5 exists
SELECT * FROM labels WHERE id = 5;

-- Check all available labels
SELECT id, name, color, is_deleted FROM labels ORDER BY id;

-- If label 5 doesn't exist, check what labels are available
-- You can use any of these IDs in your request

