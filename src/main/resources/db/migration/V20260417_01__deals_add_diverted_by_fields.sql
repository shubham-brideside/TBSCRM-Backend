-- Track who manually diverted a deal separately from generic created_by (USER/BOT + creator snapshot).
ALTER TABLE deals
ADD COLUMN diverted_by_user_id BIGINT NULL,
ADD COLUMN diverted_by_name VARCHAR(255) NULL;

ALTER TABLE deals
ADD CONSTRAINT fk_deals_diverted_by_user
FOREIGN KEY (diverted_by_user_id) REFERENCES users(id) ON DELETE SET NULL;

CREATE INDEX idx_deals_diverted_by_user_id ON deals(diverted_by_user_id);

-- Backfill from legacy creator fields for diverted deals, then clear creator snapshot so reports use the right columns.
UPDATE deals
SET diverted_by_user_id = created_by_user_id,
    diverted_by_name = created_by_name
WHERE (is_diverted = 1 OR deal_source = 'DIVERT')
  AND created_by_user_id IS NOT NULL;

UPDATE deals
SET created_by = 'BOT',
    created_by_user_id = NULL,
    created_by_name = NULL
WHERE diverted_by_user_id IS NOT NULL;
