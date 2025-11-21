-- Add referenced_pipeline_id column to deals table (stores the pipeline ID from which the deal was diverted)
ALTER TABLE deals 
ADD COLUMN IF NOT EXISTS referenced_pipeline_id BIGINT NULL;

-- Add foreign key constraint for referenced_pipeline_id
ALTER TABLE deals 
ADD CONSTRAINT FK_deals_referenced_pipeline 
FOREIGN KEY (referenced_pipeline_id) REFERENCES pipelines(id) ON DELETE SET NULL;

-- Add index for referenced_pipeline_id for lookups
CREATE INDEX IF NOT EXISTS idx_deals_referenced_pipeline_id ON deals(referenced_pipeline_id);

