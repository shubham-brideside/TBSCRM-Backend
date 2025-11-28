-- Migration to update sales_targets table to support multiple organizations and period types
-- This migration:
-- 1. Adds period_type column (defaults to MONTHLY for existing records)
-- 2. Adds period_start column (copies from month_start)
-- 3. Creates join table for sales_target_organizations
-- 4. Updates unique constraint

-- Step 1: Add period_type column with default value MONTHLY
ALTER TABLE sales_targets
    ADD COLUMN IF NOT EXISTS period_type VARCHAR(20) NOT NULL DEFAULT 'MONTHLY';

-- Step 2: Add period_start column and copy data from month_start
ALTER TABLE sales_targets
    ADD COLUMN IF NOT EXISTS period_start DATE NULL;

-- Copy existing month_start data to period_start
UPDATE sales_targets
SET period_start = month_start
WHERE period_start IS NULL;

-- Make period_start NOT NULL after data migration
ALTER TABLE sales_targets
    MODIFY COLUMN period_start DATE NOT NULL;

-- Step 3: Create join table for sales_target_organizations (many-to-many relationship)
CREATE TABLE IF NOT EXISTS sales_target_organizations (
    target_id BIGINT NOT NULL,
    organization_id BIGINT NOT NULL,
    PRIMARY KEY (target_id, organization_id),
    CONSTRAINT fk_target_org_target
        FOREIGN KEY (target_id) REFERENCES sales_targets(id) ON DELETE CASCADE,
    CONSTRAINT fk_target_org_organization
        FOREIGN KEY (organization_id) REFERENCES organizations(id) ON DELETE CASCADE,
    INDEX idx_target_org_target (target_id),
    INDEX idx_target_org_organization (organization_id)
);

-- Step 4: Drop old unique constraint and create new one
ALTER TABLE sales_targets
    DROP INDEX IF EXISTS uk_target_user_category_month;

ALTER TABLE sales_targets
    ADD CONSTRAINT uk_target_user_category_period
        UNIQUE (user_id, category, period_type, period_start);

-- Note: month_start column is kept for backward compatibility but will be deprecated
-- It can be removed in a future migration after ensuring all code uses period_start

