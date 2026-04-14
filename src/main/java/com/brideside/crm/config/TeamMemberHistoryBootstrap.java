package com.brideside.crm.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

@Component
public class TeamMemberHistoryBootstrap implements ApplicationRunner {

    private static final Logger log = LoggerFactory.getLogger(TeamMemberHistoryBootstrap.class);
    private final JdbcTemplate jdbcTemplate;

    public TeamMemberHistoryBootstrap(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    @Override
    public void run(ApplicationArguments args) {
        ensureTable();
        int inserted = backfillFromTeamsAndMembers();
        log.info("Team member history bootstrap inserted {} rows", inserted);
    }

    private void ensureTable() {
        jdbcTemplate.execute("""
                CREATE TABLE IF NOT EXISTS team_member_history (
                    id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
                    team_id BIGINT NOT NULL,
                    manager_id BIGINT NOT NULL,
                    member_id BIGINT NOT NULL,
                    effective_from DATETIME(6) NOT NULL,
                    effective_to DATETIME(6) NULL,
                    created_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
                    updated_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
                    CONSTRAINT fk_team_member_history_team FOREIGN KEY (team_id) REFERENCES teams(id) ON DELETE CASCADE,
                    CONSTRAINT fk_team_member_history_manager FOREIGN KEY (manager_id) REFERENCES users(id),
                    CONSTRAINT fk_team_member_history_member FOREIGN KEY (member_id) REFERENCES users(id)
                )
                """);
    }

    private int backfillFromTeamsAndMembers() {
        return jdbcTemplate.update("""
                INSERT INTO team_member_history (
                    team_id, manager_id, member_id, effective_from, effective_to, created_at, updated_at
                )
                SELECT tm.team_id,
                       t.manager_id,
                       tm.user_id,
                       COALESCE(t.updated_at, t.created_at, NOW(6)),
                       NULL,
                       NOW(6),
                       NOW(6)
                FROM team_members tm
                INNER JOIN teams t ON t.id = tm.team_id
                WHERE t.manager_id IS NOT NULL
                  AND tm.user_id <> t.manager_id
                  AND NOT EXISTS (
                      SELECT 1
                      FROM team_member_history h
                      WHERE h.team_id = tm.team_id
                        AND h.manager_id = t.manager_id
                        AND h.member_id = tm.user_id
                        AND h.effective_to IS NULL
                  )
                """);
    }
}
