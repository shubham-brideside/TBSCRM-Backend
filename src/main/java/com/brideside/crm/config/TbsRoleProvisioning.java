package com.brideside.crm.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/**
 * Ensures {@code roles.name} can store {@code TBS_*} values and inserts those role rows.
 * <p>
 * Many databases use {@code ENUM('ADMIN',...)} for {@code roles.name}; MySQL rejects
 * {@code INSERT} for values not listed on the ENUM. We widen the column to {@code VARCHAR}
 * when needed, matching JPA {@code @Enumerated(STRING)}.
 */
@Component
public class TbsRoleProvisioning {

    private static final Logger log = LoggerFactory.getLogger(TbsRoleProvisioning.class);

    private final JdbcTemplate jdbcTemplate;

    public TbsRoleProvisioning(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    /**
     * Idempotent: safe to call on every TBS user create and at application startup.
     */
    public void ensureTbsRolesPresent() {
        ensureRolesNameColumnAcceptsNewRoleNames();
        renameLegacyTbsRoleNames();
        insertRoleIfMissing(
                "TBS_PRESALES",
                "TBS onboarding: pre-sales; default org TBS Test; custom pipeline stages.");
        insertRoleIfMissing(
                "TBS_REL_MANAGER",
                "TBS onboarding: relationship manager; default org TBS Test; custom pipeline stages.");
        insertRoleIfMissing(
                "TBS_SVC_MANAGER",
                "TBS onboarding: service manager; org Revaah / TBS Planning / TBS Venue; custom pipeline stages.");
    }

    private void ensureRolesNameColumnAcceptsNewRoleNames() {
        Integer tableOk = jdbcTemplate.queryForObject(
                """
                SELECT COUNT(*) FROM information_schema.TABLES
                WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'roles'
                """,
                Integer.class);
        if (tableOk == null || tableOk == 0) {
            log.warn("Skipping roles.name check: roles table not found");
            return;
        }
        String columnType = jdbcTemplate.query(
                """
                SELECT COLUMN_TYPE FROM information_schema.COLUMNS
                WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'roles' AND COLUMN_NAME = 'name'
                """,
                rs -> {
                    if (!rs.next()) {
                        return null;
                    }
                    return rs.getString(1);
                });
        if (columnType == null) {
            return;
        }
        if (columnType.toLowerCase().startsWith("enum(")) {
            jdbcTemplate.execute("ALTER TABLE roles MODIFY COLUMN name VARCHAR(64) NOT NULL");
            log.info("Converted roles.name from ENUM to VARCHAR(64) for TBS and future roles");
        }
    }

    private void renameLegacyTbsRoleNames() {
        jdbcTemplate.update("UPDATE roles SET name = 'TBS_PRESALES' WHERE name = 'TBS_PRE_SALES'");
        jdbcTemplate.update("UPDATE roles SET name = 'TBS_REL_MANAGER' WHERE name = 'TBS_RELATIONSHIP_MANAGER'");
        jdbcTemplate.update("UPDATE roles SET name = 'TBS_SVC_MANAGER' WHERE name = 'TBS_SERVICE_MANAGER'");
    }

    private void insertRoleIfMissing(String name, String description) {
        Integer count = jdbcTemplate.queryForObject(
                "SELECT COUNT(*) FROM roles WHERE name = ?",
                Integer.class,
                name);
        if (count != null && count > 0) {
            return;
        }
        jdbcTemplate.update("INSERT INTO roles (name, description) VALUES (?, ?)", name, description);
        log.info("Inserted role {}", name);
    }
}
