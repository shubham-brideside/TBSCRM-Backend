-- Create label table for customizable deal labels
CREATE TABLE IF NOT EXISTS label (
    id BIGINT PRIMARY KEY AUTO_INCREMENT,
    name VARCHAR(100) NOT NULL,
    color VARCHAR(20) NOT NULL DEFAULT '#94a3b8',
    organization_id BIGINT NULL,
    pipeline_id BIGINT NULL,
    created_by_user_id BIGINT NULL,
    is_global BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);

-- Add indexes for better query performance
CREATE INDEX idx_label_organization_id ON label (organization_id);
CREATE INDEX idx_label_pipeline_id ON label (pipeline_id);
CREATE INDEX idx_label_is_global ON label (is_global);
CREATE INDEX idx_label_name ON label (name);

-- Insert default labels (migrating from enum values)
INSERT INTO label (name, color, is_global) VALUES 
    ('DIRECT', '#22c55e', TRUE),
    ('DIVERT', '#3b82f6', TRUE),
    ('DESTINATION', '#6b7280', TRUE),
    ('PARTY MAKEUP', '#f59e0b', TRUE),
    ('PRE WEDDING', '#8b5cf6', TRUE),
    ('SMA', '#1f2937', TRUE),
    ('PM', '#eab308', TRUE),
    ('SINGLE DAY', '#84cc16', TRUE),
    ('IMPORTANT', '#f59e0b', TRUE),
    ('WEDDING', '#e5e7eb', TRUE);
