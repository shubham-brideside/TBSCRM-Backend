package com.brideside.crm.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.core.annotation.Order;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/**
 * Ensures TBS user columns exist on {@code users} and TBS roles exist in {@code roles}
 * when Flyway migrations are not executed automatically.
 */
@Component
@Order(8)
public class TbsUserSchemaAndRolesBootstrap implements ApplicationRunner {

    private static final Logger log = LoggerFactory.getLogger(TbsUserSchemaAndRolesBootstrap.class);

    private final JdbcTemplate jdbcTemplate;
    private final TbsRoleProvisioning tbsRoleProvisioning;

    public TbsUserSchemaAndRolesBootstrap(JdbcTemplate jdbcTemplate, TbsRoleProvisioning tbsRoleProvisioning) {
        this.jdbcTemplate = jdbcTemplate;
        this.tbsRoleProvisioning = tbsRoleProvisioning;
    }

    @Override
    public void run(ApplicationArguments args) {
        try {
            ensureColumn("is_tbs_user", "TINYINT(1) NOT NULL DEFAULT 0");
            ensureColumn("tbs_home_organization_id", "BIGINT NULL");
            ensureColumn("tbs_default_pipeline_id", "BIGINT NULL");
            ensureForeignKey(
                    "fk_users_tbs_home_organization",
                    "tbs_home_organization_id",
                    "organizations",
                    "id");
            ensureForeignKey(
                    "fk_users_tbs_default_pipeline",
                    "tbs_default_pipeline_id",
                    "pipelines",
                    "id");
        } catch (Exception e) {
            log.warn("TBS user column/FK bootstrap failed: {}", e.toString());
        }

        try {
            tbsRoleProvisioning.ensureTbsRolesPresent();
        } catch (Exception e) {
            log.error("TBS roles bootstrap failed (first TBS create will retry provisioning): {}", e.toString(), e);
        }
    }

    private void ensureColumn(String column, String ddl) {
        Integer n = jdbcTemplate.queryForObject(
                """
                SELECT COUNT(*) FROM information_schema.COLUMNS
                WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'users' AND COLUMN_NAME = ?
                """,
                Integer.class,
                column);
        if (n != null && n > 0) {
            return;
        }
        jdbcTemplate.execute("ALTER TABLE users ADD COLUMN " + column + " " + ddl);
        log.info("Added column users.{}", column);
    }

    private void ensureForeignKey(String fkName, String column, String refTable, String refColumn) {
        Integer existing = jdbcTemplate.queryForObject(
                """
                SELECT COUNT(*) FROM information_schema.KEY_COLUMN_USAGE
                WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'users'
                  AND COLUMN_NAME = ? AND REFERENCED_TABLE_NAME = ?
                """,
                Integer.class,
                column,
                refTable);
        if (existing != null && existing > 0) {
            return;
        }
        Integer refExists = jdbcTemplate.queryForObject(
                """
                SELECT COUNT(*) FROM information_schema.TABLES
                WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = ?
                """,
                Integer.class,
                refTable);
        if (refExists == null || refExists == 0) {
            log.warn("Skipping FK {}: table {} not found", fkName, refTable);
            return;
        }
        try {
            jdbcTemplate.execute(
                    "ALTER TABLE users ADD CONSTRAINT " + fkName + " FOREIGN KEY (" + column + ") REFERENCES "
                            + refTable + " (" + refColumn + ") ON DELETE SET NULL");
            log.info("Added foreign key {} on users.{}", fkName, column);
        } catch (Exception e) {
            log.warn("Could not add FK {}: {}", fkName, e.getMessage());
        }
    }

}
