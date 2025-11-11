SET @stmt := (
    SELECT IF(
        EXISTS(SELECT 1 FROM information_schema.COLUMNS WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'pipelines' AND COLUMN_NAME = 'team'),
        'ALTER TABLE pipelines DROP COLUMN team',
        'SELECT 1'
    )
);
PREPARE drop_stmt FROM @stmt;
EXECUTE drop_stmt;
DEALLOCATE PREPARE drop_stmt;

SET @stmt := (
    SELECT IF(
        EXISTS(SELECT 1 FROM information_schema.COLUMNS WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'pipelines' AND COLUMN_NAME = 'owner_user_id'),
        'ALTER TABLE pipelines DROP COLUMN owner_user_id',
        'SELECT 1'
    )
);
PREPARE drop_stmt FROM @stmt;
EXECUTE drop_stmt;
DEALLOCATE PREPARE drop_stmt;

SET @stmt := (
    SELECT IF(
        EXISTS(SELECT 1 FROM information_schema.COLUMNS WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'pipelines' AND COLUMN_NAME = 'description'),
        'ALTER TABLE pipelines DROP COLUMN description',
        'SELECT 1'
    )
);
PREPARE drop_stmt FROM @stmt;
EXECUTE drop_stmt;
DEALLOCATE PREPARE drop_stmt;

SET @stmt := (
    SELECT IF(
        EXISTS(SELECT 1 FROM information_schema.COLUMNS WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'pipelines' AND COLUMN_NAME = 'deal_probability_enabled'),
        'ALTER TABLE pipelines DROP COLUMN deal_probability_enabled',
        'SELECT 1'
    )
);
PREPARE drop_stmt FROM @stmt;
EXECUTE drop_stmt;
DEALLOCATE PREPARE drop_stmt;

SET @stmt := (
    SELECT IF(
        EXISTS(SELECT 1 FROM information_schema.COLUMNS WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'pipelines' AND COLUMN_NAME = 'active_flag'),
        'ALTER TABLE pipelines DROP COLUMN active_flag',
        'SELECT 1'
    )
);
PREPARE drop_stmt FROM @stmt;
EXECUTE drop_stmt;
DEALLOCATE PREPARE drop_stmt;

SET @stmt := (
    SELECT IF(
        EXISTS(SELECT 1 FROM information_schema.COLUMNS WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'pipelines' AND COLUMN_NAME = 'display_order'),
        'ALTER TABLE pipelines DROP COLUMN display_order',
        'SELECT 1'
    )
);
PREPARE drop_stmt FROM @stmt;
EXECUTE drop_stmt;
DEALLOCATE PREPARE drop_stmt;
