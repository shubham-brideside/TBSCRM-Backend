-- Create custom_filters table for storing user-specific custom filter configurations
-- Supports multiple entity types: persons, deals, activities
CREATE TABLE IF NOT EXISTS custom_filters (
    id BIGINT PRIMARY KEY AUTO_INCREMENT,
    user_id BIGINT NOT NULL,
    filter_name VARCHAR(255) NOT NULL,
    entity_type VARCHAR(50) NOT NULL DEFAULT 'persons',
    conditions JSON NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    UNIQUE KEY unique_user_filter_entity (user_id, filter_name, entity_type),
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    INDEX idx_user_id (user_id),
    INDEX idx_filter_name (filter_name),
    INDEX idx_entity_type (entity_type)
);
