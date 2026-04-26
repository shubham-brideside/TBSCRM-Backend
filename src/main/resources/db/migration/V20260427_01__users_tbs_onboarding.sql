-- TBS user onboarding (MySQL). Run manually if Flyway is not enabled.
-- Schema columns and FKs are also applied at startup by TbsUserSchemaAndRolesBootstrap when Flyway is off.

-- If roles.name is ENUM(...), widen it first or INSERTs for TBS_* will fail:
-- ALTER TABLE roles MODIFY COLUMN name VARCHAR(64) NOT NULL;

-- Optional (run only if bootstrap is disabled):
-- ALTER TABLE users ADD COLUMN is_tbs_user TINYINT(1) NOT NULL DEFAULT 0;
-- ALTER TABLE users ADD COLUMN tbs_home_organization_id BIGINT NULL;
-- ALTER TABLE users ADD COLUMN tbs_default_pipeline_id BIGINT NULL;
-- ALTER TABLE users ADD CONSTRAINT fk_users_tbs_home_organization FOREIGN KEY (tbs_home_organization_id) REFERENCES organizations (id) ON DELETE SET NULL;
-- ALTER TABLE users ADD CONSTRAINT fk_users_tbs_default_pipeline FOREIGN KEY (tbs_default_pipeline_id) REFERENCES pipelines (id) ON DELETE SET NULL;

INSERT IGNORE INTO roles (name, description) VALUES
    ('TBS_PRESALES', 'TBS onboarding: pre-sales; default org TBS Test; custom pipeline stages.'),
    ('TBS_REL_MANAGER', 'TBS onboarding: relationship manager; default org TBS Test; custom pipeline stages.'),
    ('TBS_SVC_MANAGER', 'TBS onboarding: service manager; org Revaah / TBS Planning / TBS Venue; custom pipeline stages.');
