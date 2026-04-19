-- Category managers: product vertical (e.g. Photography) for pipeline/deal visibility.
ALTER TABLE users
    ADD COLUMN managed_category_id BIGINT NULL,
    ADD CONSTRAINT fk_users_managed_category FOREIGN KEY (managed_category_id) REFERENCES categories (id);
