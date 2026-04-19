-- =============================================================================
-- QUICK FIX for IDE "read-only / no corresponding table column"
-- Use this in a SQL CONSOLE — do not rely on the table grid editor.
-- Replace :category_id with categories.id and :user_id with users.id
-- =============================================================================

-- Example (edit numbers):
UPDATE users SET user_managed_category_id = 4 WHERE id = 71;

-- Verify:
SELECT id, user_managed_category_id, email FROM users WHERE id = 71;
