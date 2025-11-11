ALTER TABLE pipelines
    ADD COLUMN category VARCHAR(255) NULL AFTER name,
    ADD COLUMN team_id BIGINT NULL AFTER category,
    ADD COLUMN is_deleted TINYINT(1) NOT NULL DEFAULT 0 AFTER organization_id;


