CREATE TABLE IF NOT EXISTS organization_activation (
    id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
    organization_id BIGINT NOT NULL UNIQUE,

    contract_signed TINYINT(1) NOT NULL DEFAULT 0,
    onboarding_fee_received TINYINT(1) NOT NULL DEFAULT 0,
    gmail_and_login_credentials TINYINT(1) NOT NULL DEFAULT 0,
    phone_and_sim_issued TINYINT(1) NOT NULL DEFAULT 0,
    calendar_setup TINYINT(1) NOT NULL DEFAULT 0,
    pricing_sheet_setup TINYINT(1) NOT NULL DEFAULT 0,
    ig_contact_details TINYINT(1) NOT NULL DEFAULT 0,
    ig_vet TINYINT(1) NOT NULL DEFAULT 0,
    whatsapp_setup TINYINT(1) NOT NULL DEFAULT 0,
    hr_contract_verification TINYINT(1) NOT NULL DEFAULT 0,
    hr_phone_undertaking_form TINYINT(1) NOT NULL DEFAULT 0,
    chatbot_setup TINYINT(1) NOT NULL DEFAULT 0,
    quote_ready TINYINT(1) NOT NULL DEFAULT 0,
    client_contract_ready TINYINT(1) NOT NULL DEFAULT 0,
    ad_set_ready TINYINT(1) NOT NULL DEFAULT 0,
    ad_budget_ready TINYINT(1) NOT NULL DEFAULT 0,

    completed_count INT NOT NULL DEFAULT 0,
    total_count INT NOT NULL DEFAULT 16,
    activated TINYINT(1) NOT NULL DEFAULT 0,

    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,

    CONSTRAINT fk_organization_activation_organization
        FOREIGN KEY (organization_id) REFERENCES organizations(id)
);

