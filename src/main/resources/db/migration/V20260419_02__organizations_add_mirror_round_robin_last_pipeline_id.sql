-- Tracks last pipeline that received an auto-mirror deal for round-robin distribution across org pipelines.
ALTER TABLE organizations
    ADD COLUMN round_robin_last_pipeline_id BIGINT NULL;

ALTER TABLE organizations
    ADD CONSTRAINT fk_organizations_round_robin_last_pipeline
        FOREIGN KEY (round_robin_last_pipeline_id) REFERENCES pipelines (id)
        ON DELETE SET NULL;
