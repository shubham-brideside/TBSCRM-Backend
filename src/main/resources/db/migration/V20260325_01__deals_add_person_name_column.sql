-- Add denormalized person name snapshot to deals table
-- This lets us search/sort/display person name without joining persons.

ALTER TABLE deals
    ADD COLUMN person_name VARCHAR(255) NULL;

-- Backfill existing rows from persons
UPDATE deals d
JOIN persons p ON d.person_id = p.id
SET d.person_name = p.name
WHERE d.person_id IS NOT NULL;

CREATE INDEX idx_deals_person_name ON deals(person_name);

-- Keep deals.person_name in sync automatically
DROP TRIGGER IF EXISTS trg_deals_bi_set_person_name;
DROP TRIGGER IF EXISTS trg_deals_bu_set_person_name;

DELIMITER $$

CREATE TRIGGER trg_deals_bi_set_person_name
BEFORE INSERT ON deals
FOR EACH ROW
BEGIN
    IF NEW.person_id IS NULL THEN
        SET NEW.person_name = NULL;
    ELSE
        SET NEW.person_name = (SELECT p.name FROM persons p WHERE p.id = NEW.person_id);
    END IF;
END$$

CREATE TRIGGER trg_deals_bu_set_person_name
BEFORE UPDATE ON deals
FOR EACH ROW
BEGIN
    IF NEW.person_id IS NULL THEN
        SET NEW.person_name = NULL;
    ELSEIF NEW.person_name IS NULL OR NOT (OLD.person_id <=> NEW.person_id) THEN
        SET NEW.person_name = (SELECT p.name FROM persons p WHERE p.id = NEW.person_id);
    END IF;
END$$

DELIMITER ;

-- When a person's name changes, update all related deals
DROP TRIGGER IF EXISTS trg_persons_au_set_deals_person_name;

DELIMITER $$

CREATE TRIGGER trg_persons_au_set_deals_person_name
AFTER UPDATE ON persons
FOR EACH ROW
BEGIN
    IF NOT (NEW.name <=> OLD.name) THEN
        UPDATE deals d
        SET d.person_name = NEW.name
        WHERE d.person_id = NEW.id;
    END IF;
END$$

DELIMITER ;

