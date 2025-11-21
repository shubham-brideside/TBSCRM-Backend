-- Add source_pipeline_id column to store the initial/source pipeline
ALTER TABLE deals 
ADD COLUMN IF NOT EXISTS source_pipeline_id BIGINT NULL;

-- Add foreign key constraint for source_pipeline_id
ALTER TABLE deals 
ADD CONSTRAINT FK_deals_source_pipeline 
FOREIGN KEY (source_pipeline_id) REFERENCES pipelines(id) ON DELETE SET NULL;

-- Add pipeline_history column to store JSON array of all pipeline IDs the deal has been in
ALTER TABLE deals 
ADD COLUMN IF NOT EXISTS pipeline_history JSON NULL;

-- Add index for source_pipeline_id for lookups
CREATE INDEX IF NOT EXISTS idx_deals_source_pipeline_id ON deals(source_pipeline_id);

