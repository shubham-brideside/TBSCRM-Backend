-- Add nullable WhatsApp group created flag on deals
ALTER TABLE deals
    ADD COLUMN whatsapp_group_created BOOLEAN NULL;
