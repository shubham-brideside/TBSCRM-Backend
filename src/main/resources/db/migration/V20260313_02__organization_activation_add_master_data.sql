-- Master data checklist item (same as other tinyint flags). Total checks = 17.
ALTER TABLE organization_activation
    ADD COLUMN master_data TINYINT(1) NOT NULL DEFAULT 0 AFTER whatsapp_setup;

UPDATE organization_activation
SET total_count = 17,
    activated = CASE WHEN completed_count >= 17 THEN 1 ELSE 0 END;
