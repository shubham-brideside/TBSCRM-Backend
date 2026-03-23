-- Add nullable contract shared flag on deals
ALTER TABLE deals
    ADD COLUMN contract_shared BOOLEAN NULL;
