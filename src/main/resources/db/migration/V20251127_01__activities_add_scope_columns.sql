ALTER TABLE activities
    ADD COLUMN organization_id BIGINT NULL,
    ADD COLUMN assigned_user_id BIGINT NULL;

ALTER TABLE activities
    ADD CONSTRAINT fk_activities_organization
        FOREIGN KEY (organization_id) REFERENCES organizations(id),
    ADD CONSTRAINT fk_activities_assigned_user
        FOREIGN KEY (assigned_user_id) REFERENCES users(id);

-- Backfill organization_id by matching existing organization name to organizations.name
UPDATE activities a
JOIN organizations o ON o.name = a.organization
SET a.organization_id = o.id
WHERE a.organization IS NOT NULL
  AND a.organization_id IS NULL;

-- Backfill assigned_user_id by matching existing assignedUser email to users.email
UPDATE activities a
JOIN users u ON u.email = a.assignedUser
SET a.assigned_user_id = u.id
WHERE a.assignedUser IS NOT NULL
  AND a.assigned_user_id IS NULL;

CREATE INDEX idx_activities_organization_id ON activities (organization_id);
CREATE INDEX idx_activities_assigned_user_id ON activities (assigned_user_id);


