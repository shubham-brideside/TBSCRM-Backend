-- Backfill deals.category_id from legacy deals.category, then remove the legacy column.
-- Skips rows where legacy text is NULL, blank, or 'GENERAL' (no reliable mapping).

-- 1) Match legacy text to categories.name (case-insensitive, trimmed). One id per normalized name.
UPDATE deals d
INNER JOIN (
    SELECT MIN(c.id) AS id, LOWER(TRIM(c.name)) AS norm_name
    FROM categories c
    GROUP BY LOWER(TRIM(c.name))
) c ON c.norm_name = LOWER(TRIM(d.category))
SET d.category_id = c.id
WHERE d.category_id IS NULL
  AND d.category IS NOT NULL
  AND TRIM(d.category) <> ''
  AND UPPER(TRIM(d.category)) <> 'GENERAL';

-- 2) Enum / code aliases → Photography (categories row must exist)
UPDATE deals d
INNER JOIN categories c ON c.id = (
    SELECT MIN(c2.id) FROM categories c2 WHERE LOWER(TRIM(c2.name)) = 'photography'
)
SET d.category_id = c.id
WHERE d.category_id IS NULL
  AND d.category IS NOT NULL
  AND UPPER(TRIM(d.category)) IN ('PHOTOGRAPHY', 'PHOTO', 'PHOTOGRAPHERS');

UPDATE deals d
INNER JOIN categories c ON c.id = (
    SELECT MIN(c2.id) FROM categories c2 WHERE LOWER(TRIM(c2.name)) = 'makeup'
)
SET d.category_id = c.id
WHERE d.category_id IS NULL
  AND d.category IS NOT NULL
  AND UPPER(TRIM(d.category)) IN ('MAKEUP', 'MUA');

-- Planning & Decor: match common legacy spellings to a category whose name looks like planning + decor
UPDATE deals d
INNER JOIN categories c ON c.id = (
    SELECT MIN(c2.id)
    FROM categories c2
    WHERE LOWER(TRIM(c2.name)) IN ('planning and decor', 'planning & decor')
       OR (LOWER(TRIM(c2.name)) LIKE 'planning%' AND LOWER(TRIM(c2.name)) LIKE '%decor%')
)
SET d.category_id = c.id
WHERE d.category_id IS NULL
  AND d.category IS NOT NULL
  AND UPPER(TRIM(d.category)) IN (
      'PLANNING_AND_DECOR',
      'PLANNING & DECOR',
      'PLANNING AND DECOR',
      'PLANNING_DECOR',
      'PLANNING & DECORATION',
      'PLANNING AND DECORATION',
      'DECOR'
  );

ALTER TABLE deals DROP COLUMN category;
