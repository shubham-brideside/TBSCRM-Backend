-- organizations.is_active = 1 when organization_activation.activated = 1, else 0 (for every org that has an activation row)
UPDATE organizations o
INNER JOIN organization_activation a ON a.organization_id = o.id
SET o.is_active = IF(a.activated = 1, 1, 0);
