-- Extend organization business-unit categories and seed categories table for deals/persons.

ALTER TABLE organizations
    MODIFY COLUMN category ENUM(
        'Photography',
        'Makeup',
        'Planning and Decor',
        'Planning',
        'Decor',
        'BTS'
    ) NOT NULL;

INSERT INTO categories (name)
SELECT 'Planning'
WHERE NOT EXISTS (SELECT 1 FROM categories WHERE LOWER(TRIM(name)) = 'planning');

INSERT INTO categories (name)
SELECT 'Decor'
WHERE NOT EXISTS (SELECT 1 FROM categories WHERE LOWER(TRIM(name)) = 'decor');

INSERT INTO categories (name)
SELECT 'BTS'
WHERE NOT EXISTS (SELECT 1 FROM categories WHERE LOWER(TRIM(name)) = 'bts');
