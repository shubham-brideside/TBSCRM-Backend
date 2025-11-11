-- Ensure pipelines table exists with production-ready columns
CREATE TABLE IF NOT EXISTS pipelines (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    category VARCHAR(255),
    team VARCHAR(255),
    owner_user_id BIGINT,
    organization VARCHAR(255),
    description TEXT,
    deal_probability_enabled BOOLEAN NOT NULL DEFAULT FALSE,
    active_flag BOOLEAN NOT NULL DEFAULT TRUE,
    display_order INTEGER NOT NULL DEFAULT 0,
    created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT NOW(),
    CONSTRAINT fk_pipelines_owner FOREIGN KEY (owner_user_id) REFERENCES users(id) ON DELETE SET NULL
);

ALTER TABLE pipelines
    ADD COLUMN IF NOT EXISTS category VARCHAR(255),
    ADD COLUMN IF NOT EXISTS team VARCHAR(255),
    ADD COLUMN IF NOT EXISTS owner_user_id BIGINT,
    ADD COLUMN IF NOT EXISTS organization VARCHAR(255),
    ADD COLUMN IF NOT EXISTS description TEXT,
    ADD COLUMN IF NOT EXISTS deal_probability_enabled BOOLEAN NOT NULL DEFAULT FALSE,
    ADD COLUMN IF NOT EXISTS active_flag BOOLEAN NOT NULL DEFAULT TRUE,
    ADD COLUMN IF NOT EXISTS display_order INTEGER NOT NULL DEFAULT 0,
    ADD COLUMN IF NOT EXISTS created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT NOW(),
    ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT NOW();

ALTER TABLE pipelines
    ADD CONSTRAINT IF NOT EXISTS fk_pipelines_owner FOREIGN KEY (owner_user_id) REFERENCES users(id) ON DELETE SET NULL;

CREATE UNIQUE INDEX IF NOT EXISTS pipelines_display_order_uq ON pipelines (display_order);
CREATE INDEX IF NOT EXISTS pipelines_active_idx ON pipelines (active_flag);

UPDATE pipelines
SET
    active_flag = COALESCE(active_flag, TRUE),
    deal_probability_enabled = COALESCE(deal_probability_enabled, FALSE),
    display_order = COALESCE(display_order, 0),
    created_at = COALESCE(created_at, NOW()),
    updated_at = COALESCE(updated_at, NOW());

-- Ensure stages table exists with production-ready columns
CREATE TABLE IF NOT EXISTS stages (
    id BIGSERIAL PRIMARY KEY,
    pipeline_id BIGINT NOT NULL,
    name VARCHAR(255) NOT NULL,
    stage_order INTEGER NOT NULL,
    probability INTEGER,
    active_flag BOOLEAN NOT NULL DEFAULT TRUE,
    rotten_flag BOOLEAN NOT NULL DEFAULT FALSE,
    rotten_days INTEGER,
    created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT NOW(),
    CONSTRAINT fk_stages_pipeline FOREIGN KEY (pipeline_id) REFERENCES pipelines(id) ON DELETE CASCADE
);

ALTER TABLE stages
    ADD COLUMN IF NOT EXISTS probability INTEGER,
    ADD COLUMN IF NOT EXISTS active_flag BOOLEAN NOT NULL DEFAULT TRUE,
    ADD COLUMN IF NOT EXISTS rotten_flag BOOLEAN NOT NULL DEFAULT FALSE,
    ADD COLUMN IF NOT EXISTS rotten_days INTEGER,
    ADD COLUMN IF NOT EXISTS created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT NOW(),
    ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT NOW();

ALTER TABLE stages
    ADD CONSTRAINT IF NOT EXISTS fk_stages_pipeline FOREIGN KEY (pipeline_id) REFERENCES pipelines(id) ON DELETE CASCADE;

CREATE UNIQUE INDEX IF NOT EXISTS stages_pipeline_order_uq ON stages (pipeline_id, stage_order);
CREATE UNIQUE INDEX IF NOT EXISTS stages_pipeline_lower_name_uq ON stages (pipeline_id, lower(name));

UPDATE stages
SET
    active_flag = COALESCE(active_flag, TRUE),
    rotten_flag = COALESCE(rotten_flag, FALSE),
    created_at = COALESCE(created_at, NOW()),
    updated_at = COALESCE(updated_at, NOW());

