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
);

CREATE INDEX idx_team_member_history_team_active ON team_member_history(team_id, effective_to);
CREATE INDEX idx_team_member_history_manager_window ON team_member_history(manager_id, effective_from, effective_to);
CREATE INDEX idx_team_member_history_member_window ON team_member_history(member_id, effective_from, effective_to);

INSERT INTO team_member_history (team_id, manager_id, member_id, effective_from, effective_to)
SELECT tm.team_id,
       t.manager_id,
       tm.user_id,
       NOW(6),
       NULL
FROM team_members tm
INNER JOIN teams t ON t.id = tm.team_id
INNER JOIN users u ON u.id = tm.user_id
INNER JOIN roles r ON r.id = u.role_id
WHERE t.manager_id IS NOT NULL
  AND r.name = 'PRESALES'
  AND NOT EXISTS (
      SELECT 1
      FROM team_member_history h
      WHERE h.team_id = tm.team_id
        AND h.manager_id = t.manager_id
        AND h.member_id = tm.user_id
        AND h.effective_to IS NULL
  );
