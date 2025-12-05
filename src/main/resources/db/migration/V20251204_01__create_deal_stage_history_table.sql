-- Create deal_stage_history table to track when a deal enters and exits each stage
CREATE TABLE IF NOT EXISTS deal_stage_history (
    id BIGINT PRIMARY KEY AUTO_INCREMENT,
    deal_id BIGINT NOT NULL,
    stage_id BIGINT NOT NULL,
    entered_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    exited_at TIMESTAMP NULL,
    days_in_stage INT NULL,
    is_current BIT(1) NOT NULL DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    
    CONSTRAINT FK_deal_stage_history_deal FOREIGN KEY (deal_id) REFERENCES deals(id) ON DELETE CASCADE,
    CONSTRAINT FK_deal_stage_history_stage FOREIGN KEY (stage_id) REFERENCES stages(id) ON DELETE CASCADE,
    
    INDEX idx_deal_id (deal_id),
    INDEX idx_stage_id (stage_id),
    INDEX idx_is_current (is_current),
    INDEX idx_deal_stage (deal_id, stage_id)
);

