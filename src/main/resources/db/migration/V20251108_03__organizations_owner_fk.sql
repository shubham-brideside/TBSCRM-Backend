ALTER TABLE organizations
    ADD COLUMN IF NOT EXISTS owner_id BIGINT NULL AFTER name;

ALTER TABLE organizations
    ADD CONSTRAINT fk_organizations_owner_user
        FOREIGN KEY (owner_id) REFERENCES users(id);


