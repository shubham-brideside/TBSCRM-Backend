package com.brideside.crm.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.core.annotation.Order;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/**
 * Ensures {@code users.user_managed_category_id} exists (FK to {@code categories.id}).
 * Older schemas used {@code managed_category_id}; this runner renames or adds as needed when
 * Flyway migrations are not executed automatically.
 */
@Component
@Order(1)
public class UsersManagedCategorySchemaBootstrap implements ApplicationRunner {

    private static final Logger log = LoggerFactory.getLogger(UsersManagedCategorySchemaBootstrap.class);

    private static final String COL_NEW = "user_managed_category_id";
    private static final String COL_OLD = "managed_category_id";
    private static final String FK_NEW = "fk_users_user_managed_category";

    private final JdbcTemplate jdbcTemplate;

    private boolean changedThisRun;

    public UsersManagedCategorySchemaBootstrap(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    @Override
    public void run(ApplicationArguments args) {
        try {
            changedThisRun = false;
            int hasNew = countColumn(COL_NEW);
            int hasOld = countColumn(COL_OLD);
            // Hibernate may add the new column before this runs, leaving both — merge then drop legacy.
            if (hasOld > 0 && hasNew > 0) {
                copyOldColumnIntoNewThenDropOld();
            } else if (hasOld > 0) {
                migrateOldColumnToNew();
            } else if (hasNew > 0) {
                ensureForeignKeyOnNewColumn();
            } else {
                addNewColumn();
                ensureForeignKeyOnNewColumn();
            }
            if (changedThisRun) {
                log.info(
                        "users.{} schema updated. Synchronize your DB tool or use PATCH /api/users/me/managed-category.",
                        COL_NEW);
            }
        } catch (Exception e) {
            log.warn("users.{} bootstrap failed: {}", COL_NEW, e.toString());
        }
    }

    private void copyOldColumnIntoNewThenDropOld() {
        jdbcTemplate.execute(
                "UPDATE users SET " + COL_NEW + " = " + COL_OLD + " WHERE " + COL_OLD + " IS NOT NULL");
        dropForeignKeyReferencingColumn(COL_OLD);
        jdbcTemplate.execute("ALTER TABLE users DROP COLUMN " + COL_OLD);
        changedThisRun = true;
        log.info("Copied {} -> {}, dropped legacy {}", COL_OLD, COL_NEW, COL_OLD);
        ensureForeignKeyOnNewColumn();
    }

    private int countColumn(String columnName) {
        Integer n = jdbcTemplate.queryForObject(
                """
                SELECT COUNT(*) FROM information_schema.COLUMNS
                WHERE TABLE_SCHEMA = DATABASE()
                  AND TABLE_NAME = 'users'
                  AND COLUMN_NAME = ?
                """,
                Integer.class,
                columnName);
        return n == null ? 0 : n;
    }

    private void migrateOldColumnToNew() {
        dropForeignKeyReferencingColumn(COL_OLD);
        jdbcTemplate.execute(
                "ALTER TABLE users CHANGE COLUMN " + COL_OLD + " " + COL_NEW + " BIGINT NULL");
        changedThisRun = true;
        log.info("Renamed users.{} -> users.{} (data preserved)", COL_OLD, COL_NEW);
        ensureForeignKeyOnNewColumn();
    }

    private void addNewColumn() {
        jdbcTemplate.execute("ALTER TABLE users ADD COLUMN " + COL_NEW + " BIGINT NULL");
        changedThisRun = true;
        log.info("Added column users.{}", COL_NEW);
    }

    private void dropForeignKeyReferencingColumn(String columnName) {
        String fkName = jdbcTemplate.query(
                """
                SELECT kcu.CONSTRAINT_NAME
                FROM information_schema.KEY_COLUMN_USAGE kcu
                JOIN information_schema.TABLE_CONSTRAINTS tc
                  ON kcu.CONSTRAINT_SCHEMA = tc.CONSTRAINT_SCHEMA
                 AND kcu.CONSTRAINT_NAME = tc.CONSTRAINT_NAME
                WHERE kcu.TABLE_SCHEMA = DATABASE()
                  AND kcu.TABLE_NAME = 'users'
                  AND kcu.COLUMN_NAME = ?
                  AND kcu.REFERENCED_TABLE_NAME IS NOT NULL
                  AND tc.CONSTRAINT_TYPE = 'FOREIGN KEY'
                LIMIT 1
                """,
                rs -> rs.next() ? rs.getString(1) : null,
                columnName);
        if (fkName != null) {
            jdbcTemplate.execute("ALTER TABLE users DROP FOREIGN KEY `" + fkName + "`");
            log.debug("Dropped foreign key {} on users.{}", fkName, columnName);
        }
    }

    private void ensureForeignKeyOnNewColumn() {
        Integer fk = jdbcTemplate.queryForObject(
                """
                SELECT COUNT(*) FROM information_schema.KEY_COLUMN_USAGE
                WHERE TABLE_SCHEMA = DATABASE()
                  AND TABLE_NAME = 'users'
                  AND COLUMN_NAME = ?
                  AND REFERENCED_TABLE_NAME = 'categories'
                """,
                Integer.class,
                COL_NEW);
        if (fk != null && fk > 0) {
            return;
        }
        Integer categoriesTable = jdbcTemplate.queryForObject(
                """
                SELECT COUNT(*) FROM information_schema.TABLES
                WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'categories'
                """,
                Integer.class);
        if (categoriesTable == null || categoriesTable == 0) {
            log.warn("Skipping {}: categories table not found", FK_NEW);
            return;
        }
        try {
            jdbcTemplate.execute(
                    """
                    ALTER TABLE users
                        ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES categories (id)
                    """.formatted(FK_NEW, COL_NEW));
            log.info("Added foreign key {} on users.{}", FK_NEW, COL_NEW);
        } catch (Exception e) {
            log.warn("Could not add {} (may already exist): {}", FK_NEW, e.getMessage());
        }
    }
}
