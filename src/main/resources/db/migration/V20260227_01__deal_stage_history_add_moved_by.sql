-- Track who moved a deal into a stage (for reporting/audit)
ALTER TABLE deal_stage_history
    ADD COLUMN moved_by_user_id BIGINT NULL,
    ADD COLUMN moved_by_name VARCHAR(255) NULL;

CREATE INDEX idx_deal_stage_history_moved_by_user_id ON deal_stage_history (moved_by_user_id);

ALTER TABLE deal_stage_history
    ADD CONSTRAINT FK_deal_stage_history_moved_by_user
        FOREIGN KEY (moved_by_user_id) REFERENCES users(id)
        ON DELETE SET NULL;

