INSERT INTO team_member_history (team_id, manager_id, member_id, effective_from, effective_to)
SELECT tm.team_id,
       t.manager_id,
       tm.user_id,
       COALESCE(t.updated_at, t.created_at, NOW(6)),
       NULL
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
  );
