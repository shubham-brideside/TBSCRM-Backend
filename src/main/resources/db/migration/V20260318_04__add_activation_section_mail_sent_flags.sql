-- Activation checklist section email tracking flags
ALTER TABLE organization_activation
  ADD COLUMN mail_contract_section_sent TINYINT(1) NOT NULL DEFAULT 0,
  ADD COLUMN mail_calendar_ig_whatsapp_section_sent TINYINT(1) NOT NULL DEFAULT 0,
  ADD COLUMN mail_hr_section_sent TINYINT(1) NOT NULL DEFAULT 0,
  ADD COLUMN mail_chatbot_section_sent TINYINT(1) NOT NULL DEFAULT 0,
  ADD COLUMN mail_quote_client_contract_section_sent TINYINT(1) NOT NULL DEFAULT 0;

