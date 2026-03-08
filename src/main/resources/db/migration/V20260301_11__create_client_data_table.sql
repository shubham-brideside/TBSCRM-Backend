-- Client data table: stores PDF URLs (quote format, client contract format) per organization.
-- PDFs are uploaded to Azure Blob Storage; this table stores the public URLs.
-- One row per organization.
CREATE TABLE client_data (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    organization_id BIGINT NOT NULL,
    quote_format_url TEXT NULL,
    client_contract_format_url TEXT NULL,
    created_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    updated_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
    CONSTRAINT fk_client_data_organization FOREIGN KEY (organization_id) REFERENCES organizations(id) ON DELETE CASCADE,
    CONSTRAINT uq_client_data_organization UNIQUE (organization_id)
);

CREATE INDEX idx_client_data_organization_id ON client_data(organization_id);
