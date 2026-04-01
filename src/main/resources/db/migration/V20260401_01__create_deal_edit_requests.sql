CREATE TABLE deal_edit_requests (
    id BIGINT PRIMARY KEY AUTO_INCREMENT,
    deal_id BIGINT NOT NULL,
    requested_by_user_id BIGINT NOT NULL,
    reason TEXT NULL,
    requested_changes JSON NOT NULL,
    status VARCHAR(20) NOT NULL DEFAULT 'PENDING',
    processed_by_user_id BIGINT NULL,
    processed_at DATETIME NULL,
    admin_comment TEXT NULL,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NULL DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP,

    CONSTRAINT fk_deal_edit_requests_deal
        FOREIGN KEY (deal_id) REFERENCES deals(id),

    CONSTRAINT fk_deal_edit_requests_requested_by
        FOREIGN KEY (requested_by_user_id) REFERENCES users(id),

    CONSTRAINT fk_deal_edit_requests_processed_by
        FOREIGN KEY (processed_by_user_id) REFERENCES users(id)
);

