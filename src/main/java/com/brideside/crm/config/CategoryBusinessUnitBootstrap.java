package com.brideside.crm.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.core.annotation.Order;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/**
 * Applies Planning / Decor / BTS category changes at startup. SQL files under
 * {@code db/migration} are not executed unless Flyway/Liquibase is configured; this
 * runner keeps the schema and {@code categories} rows in sync idempotently.
 */
@Component
@Order(0)
public class CategoryBusinessUnitBootstrap implements ApplicationRunner {

    private static final Logger log = LoggerFactory.getLogger(CategoryBusinessUnitBootstrap.class);

    private final JdbcTemplate jdbcTemplate;

    public CategoryBusinessUnitBootstrap(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    @Override
    public void run(ApplicationArguments args) {
        try {
            extendOrganizationCategoryEnumIfNeeded();
            seedCategoriesIfNeeded();
        } catch (Exception e) {
            log.warn("Category business-unit bootstrap failed: {}", e.toString());
        }
    }

    private void extendOrganizationCategoryEnumIfNeeded() {
        String columnType = jdbcTemplate.query(
                "SELECT COLUMN_TYPE FROM information_schema.COLUMNS "
                        + "WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organizations' AND COLUMN_NAME = 'category'",
                rs -> rs.next() ? rs.getString(1) : null);
        if (columnType == null || !columnType.toLowerCase().contains("enum")) {
            log.debug("organizations.category is missing or not a MySQL ENUM; skipping enum extension");
            return;
        }
        if (columnType.contains("'BTS'")) {
            return;
        }
        jdbcTemplate.execute(
                """
                ALTER TABLE organizations
                    MODIFY COLUMN category ENUM(
                        'Photography',
                        'Makeup',
                        'Planning and Decor',
                        'Planning',
                        'Decor',
                        'BTS'
                    ) NOT NULL
                """);
        log.info("Extended organizations.category ENUM with Planning, Decor, BTS");
    }

    private void seedCategoriesIfNeeded() {
        jdbcTemplate.update(
                """
                INSERT INTO categories (name)
                SELECT 'Planning'
                WHERE NOT EXISTS (SELECT 1 FROM categories WHERE LOWER(TRIM(name)) = 'planning')
                """);
        jdbcTemplate.update(
                """
                INSERT INTO categories (name)
                SELECT 'Decor'
                WHERE NOT EXISTS (SELECT 1 FROM categories WHERE LOWER(TRIM(name)) = 'decor')
                """);
        jdbcTemplate.update(
                """
                INSERT INTO categories (name)
                SELECT 'BTS'
                WHERE NOT EXISTS (SELECT 1 FROM categories WHERE LOWER(TRIM(name)) = 'bts')
                """);
        log.info("Ensured Planning, Decor, BTS rows exist in categories table (if table is present)");
    }
}
