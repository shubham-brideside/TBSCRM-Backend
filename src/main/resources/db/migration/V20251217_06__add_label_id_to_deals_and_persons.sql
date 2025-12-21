-- Add label_id foreign key columns to deals and persons tables
-- This allows storing a single custom label from the labels table

-- Add label_id to deals table
ALTER TABLE deals 
ADD COLUMN label_id BIGINT NULL,
ADD FOREIGN KEY (label_id) REFERENCES labels(id) ON DELETE SET NULL;

-- Add label_id to persons table
ALTER TABLE persons 
ADD COLUMN label_id BIGINT NULL,
ADD FOREIGN KEY (label_id) REFERENCES labels(id) ON DELETE SET NULL;

-- Create indexes for better query performance
CREATE INDEX idx_deals_label_id ON deals(label_id);
CREATE INDEX idx_persons_label_id ON persons(label_id);

