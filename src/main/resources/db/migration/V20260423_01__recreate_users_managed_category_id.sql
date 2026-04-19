-- Recreates users.managed_category_id from scratch (drops FK + column, then re-adds).
-- Run manually against your MySQL database if Flyway is not enabled (IntelliJ Database console, mysql CLI, etc.).
-- WARNING: Clears all values in managed_category_id.

-- Drop foreign key on managed_category_id if one exists (name may be fk_users_managed_category or auto-generated).
SET @dbname = DATABASE();
SET @fkname = (
    SELECT kcu.CONSTRAINT_NAME
    FROM information_schema.KEY_COLUMN_USAGE kcu
    JOIN information_schema.TABLE_CONSTRAINTS tc
      ON kcu.CONSTRAINT_SCHEMA = tc.CONSTRAINT_SCHEMA
     AND kcu.CONSTRAINT_NAME = tc.CONSTRAINT_NAME
    WHERE kcu.TABLE_SCHEMA = @dbname
      AND kcu.TABLE_NAME = 'users'
      AND kcu.COLUMN_NAME = 'managed_category_id'
      AND kcu.REFERENCED_TABLE_NAME IS NOT NULL
      AND tc.CONSTRAINT_TYPE = 'FOREIGN KEY'
    LIMIT 1
);

SET @dropfk = IF(@fkname IS NOT NULL,
    CONCAT('ALTER TABLE users DROP FOREIGN KEY `', @fkname, '`'),
    'SELECT 1');
PREPARE stmt_drop_fk FROM @dropfk;
EXECUTE stmt_drop_fk;
DEALLOCATE PREPARE stmt_drop_fk;

-- Drop column (fails if column does not exist — only run when the column is present)
ALTER TABLE users DROP COLUMN managed_category_id;

-- Re-create nullable FK to categories
ALTER TABLE users ADD COLUMN managed_category_id BIGINT NULL;

ALTER TABLE users
    ADD CONSTRAINT fk_users_managed_category FOREIGN KEY (managed_category_id) REFERENCES categories (id);
