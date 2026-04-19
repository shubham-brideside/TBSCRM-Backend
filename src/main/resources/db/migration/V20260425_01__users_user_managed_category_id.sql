-- Replace managed_category_id with user_managed_category_id (same FK to categories.id; data preserved via CHANGE COLUMN).
-- Run manually if Flyway is not enabled (same as other migration files in this project).
-- Preconditions: column managed_category_id exists and user_managed_category_id does not yet (skip if already migrated).

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

SET @dropfk = IF(@fkname IS NOT NULL, CONCAT('ALTER TABLE users DROP FOREIGN KEY `', @fkname, '`'), 'SELECT 1');
PREPARE s FROM @dropfk;
EXECUTE s;
DEALLOCATE PREPARE s;

ALTER TABLE users CHANGE COLUMN managed_category_id user_managed_category_id BIGINT NULL;

ALTER TABLE users
    ADD CONSTRAINT fk_users_user_managed_category FOREIGN KEY (user_managed_category_id) REFERENCES categories (id);
