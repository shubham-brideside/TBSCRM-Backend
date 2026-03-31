-- Denormalized deal owner: mirrors persons.owner_id when a person is linked, else organizations.owner_id.
-- Kept in sync via triggers (insert/update on deals, person/org owner changes).

ALTER TABLE deals
    ADD COLUMN owner_id BIGINT NULL;

UPDATE deals d
JOIN persons p ON d.person_id = p.id
SET d.owner_id = p.owner_id
WHERE d.person_id IS NOT NULL;

UPDATE deals d
JOIN organizations o ON d.organization_id = o.id
SET d.owner_id = o.owner_id
WHERE d.owner_id IS NULL AND d.organization_id IS NOT NULL;

CREATE INDEX idx_deals_owner_id ON deals(owner_id);

ALTER TABLE deals
    ADD CONSTRAINT fk_deals_owner FOREIGN KEY (owner_id) REFERENCES users(id) ON DELETE SET NULL;

DROP TRIGGER IF EXISTS trg_deals_bi_set_owner_id;
DROP TRIGGER IF EXISTS trg_deals_bu_set_owner_id;

DELIMITER $$

CREATE TRIGGER trg_deals_bi_set_owner_id
BEFORE INSERT ON deals
FOR EACH ROW
BEGIN
    DECLARE po BIGINT;
    SET po = NULL;
    IF NEW.person_id IS NOT NULL THEN
        SELECT p.owner_id INTO po FROM persons p WHERE p.id = NEW.person_id LIMIT 1;
    END IF;
    IF po IS NOT NULL THEN
        SET NEW.owner_id = po;
    ELSEIF NEW.organization_id IS NOT NULL THEN
        SET NEW.owner_id = (SELECT o.owner_id FROM organizations o WHERE o.id = NEW.organization_id LIMIT 1);
    END IF;
END$$

CREATE TRIGGER trg_deals_bu_set_owner_id
BEFORE UPDATE ON deals
FOR EACH ROW
BEGIN
    DECLARE po BIGINT;
    -- Respect explicit owner changes on deal updates.
    IF (OLD.owner_id <=> NEW.owner_id) THEN
        SET po = NULL;
        IF NEW.person_id IS NOT NULL THEN
            SELECT p.owner_id INTO po FROM persons p WHERE p.id = NEW.person_id LIMIT 1;
        END IF;
        IF po IS NOT NULL THEN
            SET NEW.owner_id = po;
        ELSEIF NEW.organization_id IS NOT NULL THEN
            SET NEW.owner_id = (SELECT o.owner_id FROM organizations o WHERE o.id = NEW.organization_id LIMIT 1);
        ELSE
            SET NEW.owner_id = NULL;
        END IF;
    END IF;
END$$

DELIMITER ;

DROP TRIGGER IF EXISTS trg_persons_au_set_deals_owner_id;

DELIMITER $$

CREATE TRIGGER trg_persons_au_set_deals_owner_id
AFTER UPDATE ON persons
FOR EACH ROW
BEGIN
    IF NOT (NEW.owner_id <=> OLD.owner_id) THEN
        UPDATE deals d SET d.owner_id = NEW.owner_id WHERE d.person_id = NEW.id;
    END IF;
END$$

DELIMITER ;

DROP TRIGGER IF EXISTS trg_organizations_au_set_deals_owner_id;

DELIMITER $$

CREATE TRIGGER trg_organizations_au_set_deals_owner_id
AFTER UPDATE ON organizations
FOR EACH ROW
BEGIN
    IF NOT (NEW.owner_id <=> OLD.owner_id) THEN
        UPDATE deals d
        LEFT JOIN persons p ON d.person_id = p.id
        SET d.owner_id = NEW.owner_id
        WHERE d.organization_id = NEW.id
          AND (d.person_id IS NULL OR p.owner_id IS NULL);
    END IF;
END$$

DELIMITER ;
