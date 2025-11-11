ALTER TABLE pipelines
    DROP COLUMN IF EXISTS team,
    DROP COLUMN IF EXISTS owner_user_id,
    DROP COLUMN IF EXISTS description,
    DROP COLUMN IF EXISTS deal_probability_enabled,
    DROP COLUMN IF EXISTS active_flag,
    DROP COLUMN IF EXISTS display_order;


