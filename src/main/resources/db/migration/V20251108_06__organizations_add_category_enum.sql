ALTER TABLE organizations
    ADD COLUMN IF NOT EXISTS category ENUM('Photography','Makeup','Planning and Decor') NULL AFTER owner_id;

UPDATE organizations
SET category = 'Photography'
WHERE category IS NULL;

ALTER TABLE organizations
    MODIFY COLUMN category ENUM('Photography','Makeup','Planning and Decor') NOT NULL;


