-- Create labels table for customizable labels
CREATE TABLE IF NOT EXISTS labels (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100) NOT NULL UNIQUE,
    code VARCHAR(50) NULL,
    color VARCHAR(7),
    is_deleted BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);

-- Create join table for deal-labels many-to-many relationship
-- Note: Join tables don't need created_at columns
CREATE TABLE IF NOT EXISTS deal_labels (
    deal_id BIGINT NOT NULL,
    label_id BIGINT NOT NULL,
    PRIMARY KEY (deal_id, label_id),
    FOREIGN KEY (deal_id) REFERENCES deals(id) ON DELETE CASCADE,
    FOREIGN KEY (label_id) REFERENCES labels(id) ON DELETE CASCADE
);

-- Create join table for person-labels many-to-many relationship
-- Note: Join tables don't need created_at columns
CREATE TABLE IF NOT EXISTS person_labels (
    person_id BIGINT NOT NULL,
    label_id BIGINT NOT NULL,
    PRIMARY KEY (person_id, label_id),
    FOREIGN KEY (person_id) REFERENCES persons(id) ON DELETE CASCADE,
    FOREIGN KEY (label_id) REFERENCES labels(id) ON DELETE CASCADE
);

-- Create indexes for better query performance
CREATE INDEX idx_deal_labels_deal_id ON deal_labels(deal_id);
CREATE INDEX idx_deal_labels_label_id ON deal_labels(label_id);
CREATE INDEX idx_person_labels_person_id ON person_labels(person_id);
CREATE INDEX idx_person_labels_label_id ON person_labels(label_id);
CREATE INDEX idx_labels_name ON labels(name);

-- Seed with default labels based on existing enum values
INSERT INTO labels (name, code, color, is_deleted) VALUES 
    ('Direct', 'DIRECT', '#4CAF50', FALSE),
    ('Divert', 'DIVERT', '#FF9800', FALSE),
    ('Destination', 'DESTINATION', '#2196F3', FALSE),
    ('Party Makeup', 'PARTY_MAKEUP', '#E91E63', FALSE),
    ('Pre Wedding', 'PRE_WEDDING', '#9C27B0', FALSE),
    ('Bridal Makeup', 'BRIDAL_MAKEUP', '#F44336', FALSE),
    ('Engagement', 'ENGAGEMENT', '#00BCD4', FALSE),
    ('Reception', 'RECEPTION', '#795548', FALSE),
    ('Other', 'OTHER', '#607D8B', FALSE)
ON DUPLICATE KEY UPDATE name = name;

