-- Remove deal approval flag (reverts V20260327_01).

ALTER TABLE deals
    DROP COLUMN approved;
