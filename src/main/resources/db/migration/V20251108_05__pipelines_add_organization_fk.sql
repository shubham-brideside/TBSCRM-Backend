ALTER TABLE pipelines
    ADD COLUMN IF NOT EXISTS organization_id BIGINT NULL AFTER team;

UPDATE pipelines p
        JOIN organizations o ON o.name = p.organization
SET p.organization_id = o.id
WHERE p.organization IS NOT NULL
  AND p.organization_id IS NULL;

ALTER TABLE pipelines
    ADD CONSTRAINT fk_pipelines_organization
        FOREIGN KEY (organization_id) REFERENCES organizations(id);

ALTER TABLE pipelines
    DROP COLUMN IF EXISTS organization;


