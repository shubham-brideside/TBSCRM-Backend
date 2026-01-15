-- Add entity_type column to custom_filters table to support filters for different entities
ALTER TABLE custom_filters
    ADD COLUMN IF NOT EXISTS entity_type VARCHAR(50) NOT NULL DEFAULT 'persons';

-- Update unique constraint to include entity_type (user can have same filter name for different entities)
ALTER TABLE custom_filters
    DROP INDEX IF EXISTS unique_user_filter;

ALTER TABLE custom_filters
    ADD UNIQUE KEY unique_user_filter_entity (user_id, filter_name, entity_type);

-- Add index for entity_type for better query performance
CREATE INDEX IF NOT EXISTS idx_entity_type ON custom_filters(entity_type);
