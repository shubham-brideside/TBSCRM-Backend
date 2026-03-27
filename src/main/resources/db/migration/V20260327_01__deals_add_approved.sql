-- Add approved flag for deals.
-- Defaults to false for all existing and new records.

ALTER TABLE deals
    ADD COLUMN approved BOOLEAN NOT NULL DEFAULT FALSE;
