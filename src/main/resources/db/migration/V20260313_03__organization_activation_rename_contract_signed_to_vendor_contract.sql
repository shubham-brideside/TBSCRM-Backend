-- Align with frontend "Vendor Contract" (same column, renamed only).
ALTER TABLE organization_activation
    CHANGE COLUMN contract_signed vendor_contract TINYINT(1) NOT NULL DEFAULT 0;
