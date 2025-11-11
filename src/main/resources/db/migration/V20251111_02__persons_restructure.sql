ALTER TABLE persons
    DROP COLUMN IF EXISTS wedding_date,
    DROP COLUMN IF EXISTS venue,
    DROP COLUMN IF EXISTS organization,
    DROP COLUMN IF EXISTS manager,
    DROP COLUMN IF EXISTS category,
    DROP COLUMN IF EXISTS source,
    DROP COLUMN IF EXISTS created_date,
    DROP COLUMN IF EXISTS event_type;

ALTER TABLE persons
    ADD COLUMN IF NOT EXISTS email VARCHAR(255) NULL,
    ADD COLUMN IF NOT EXISTS lead_date DATE NULL,
    ADD COLUMN IF NOT EXISTS organization_id BIGINT NULL,
    ADD COLUMN IF NOT EXISTS owner_id BIGINT NULL,
    ADD COLUMN IF NOT EXISTS label VARCHAR(50) NULL,
    ADD COLUMN IF NOT EXISTS person_source VARCHAR(50) NULL;

UPDATE persons
SET lead_date = COALESCE(lead_date, DATE(created_at));

ALTER TABLE persons
    MODIFY lead_date DATE NULL DEFAULT CURRENT_DATE;

ALTER TABLE persons
    ADD CONSTRAINT fk_persons_organization FOREIGN KEY (organization_id) REFERENCES organizations(id);

ALTER TABLE persons
    ADD CONSTRAINT fk_persons_owner FOREIGN KEY (owner_id) REFERENCES users(id);

