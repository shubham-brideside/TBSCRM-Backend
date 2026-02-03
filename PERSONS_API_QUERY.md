# Persons API Query Documentation

## Endpoint
`GET /api/persons?q=strangelybornhuman&page=0&size=50&sort=name,asc`

## Query Breakdown

### Request Parameters
- `q` = "strangelybornhuman" (search term)
- `page` = 0 (first page)
- `size` = 50 (50 records per page)
- `sort` = "name,asc" (sort by name ascending)

---

## Generated SQL Query

### For Admin Portal (No Role-Based Filtering)

```sql
SELECT 
    p.id,
    p.name,
    p.instagram_id,
    p.phone,
    p.email,
    p.lead_date,
    p.venue,
    p.city,
    p.wedding_date,
    p.organization_id,
    p.owner_id,
    p.category_id,
    p.label_enum,
    p.label_id,
    p.source,
    p.sub_source,
    p.created_at,
    p.updated_at,
    p.is_deleted
FROM persons p
LEFT JOIN organizations o ON p.organization_id = o.id
LEFT JOIN users u ON p.owner_id = u.id
WHERE (
    p.is_deleted IS NULL 
    OR p.is_deleted = false
)
AND (
    LOWER(p.name) LIKE '%strangelybornhuman%'
    OR LOWER(p.instagram_id) LIKE '%strangelybornhuman%'
    OR LOWER(p.phone) LIKE '%strangelybornhuman%'
    OR LOWER(p.email) LIKE '%strangelybornhuman%'
    OR LOWER(o.name) LIKE '%strangelybornhuman%'
    OR LOWER(u.first_name) LIKE '%strangelybornhuman%'
    OR LOWER(u.last_name) LIKE '%strangelybornhuman%'
)
ORDER BY p.name ASC
LIMIT 50 OFFSET 0;
```

### For Manager Portal (With Role-Based Filtering)

The query includes additional role-based filtering based on the logged-in user's role:

#### For SALES Manager:
```sql
SELECT 
    p.id,
    p.name,
    p.instagram_id,
    p.phone,
    p.email,
    p.lead_date,
    p.venue,
    p.city,
    p.wedding_date,
    p.organization_id,
    p.owner_id,
    p.category_id,
    p.label_enum,
    p.label_id,
    p.source,
    p.sub_source,
    p.created_at,
    p.updated_at,
    p.is_deleted
FROM persons p
LEFT JOIN organizations o ON p.organization_id = o.id
LEFT JOIN users u ON p.owner_id = u.id
WHERE (
    p.is_deleted IS NULL 
    OR p.is_deleted = false
)
AND (
    LOWER(p.name) LIKE '%strangelybornhuman%'
    OR LOWER(p.instagram_id) LIKE '%strangelybornhuman%'
    OR LOWER(p.phone) LIKE '%strangelybornhuman%'
    OR LOWER(p.email) LIKE '%strangelybornhuman%'
    OR LOWER(o.name) LIKE '%strangelybornhuman%'
    OR LOWER(u.first_name) LIKE '%strangelybornhuman%'
    OR LOWER(u.last_name) LIKE '%strangelybornhuman%'
)
AND (
    -- Condition 1: Person's organization is linked to pipelines the manager has access to
    p.organization_id IN (
        SELECT DISTINCT pip.organization_id 
        FROM pipelines pip
        INNER JOIN teams t ON pip.team_id = t.id
        WHERE t.manager_id = :currentUserId
        AND (pip.is_deleted = false OR pip.is_deleted IS NULL)
    )
    OR
    -- Condition 2: Person has deals in pipelines the manager has access to
    p.id IN (
        SELECT DISTINCT d.person_id
        FROM deals d
        INNER JOIN pipelines pip ON d.pipeline_id = pip.id
        INNER JOIN teams t ON pip.team_id = t.id
        WHERE t.manager_id = :currentUserId
        AND (d.is_deleted = false OR d.is_deleted IS NULL)
        AND (pip.is_deleted = false OR pip.is_deleted IS NULL)
        AND d.person_id IS NOT NULL
        AND d.pipeline_id IS NOT NULL
    )
)
ORDER BY p.name ASC
LIMIT 50 OFFSET 0;
```

#### For PRESALES User:
```sql
SELECT 
    p.id,
    p.name,
    p.instagram_id,
    p.phone,
    p.email,
    p.lead_date,
    p.venue,
    p.city,
    p.wedding_date,
    p.organization_id,
    p.owner_id,
    p.category_id,
    p.label_enum,
    p.label_id,
    p.source,
    p.sub_source,
    p.created_at,
    p.updated_at,
    p.is_deleted
FROM persons p
LEFT JOIN organizations o ON p.organization_id = o.id
LEFT JOIN users u ON p.owner_id = u.id
WHERE (
    p.is_deleted IS NULL 
    OR p.is_deleted = false
)
AND (
    LOWER(p.name) LIKE '%strangelybornhuman%'
    OR LOWER(p.instagram_id) LIKE '%strangelybornhuman%'
    OR LOWER(p.phone) LIKE '%strangelybornhuman%'
    OR LOWER(p.email) LIKE '%strangelybornhuman%'
    OR LOWER(o.name) LIKE '%strangelybornhuman%'
    OR LOWER(u.first_name) LIKE '%strangelybornhuman%'
    OR LOWER(u.last_name) LIKE '%strangelybornhuman%'
)
AND (
    -- Condition 1: Person's organization is linked to pipelines in teams where PRESALES is a member
    p.organization_id IN (
        SELECT DISTINCT pip.organization_id 
        FROM pipelines pip
        INNER JOIN teams t ON pip.team_id = t.id
        INNER JOIN team_members tm ON t.id = tm.team_id
        WHERE tm.user_id = :currentUserId
        AND (pip.is_deleted = false OR pip.is_deleted IS NULL)
    )
    OR
    -- Condition 2: Person has deals in pipelines in teams where PRESALES is a member
    p.id IN (
        SELECT DISTINCT d.person_id
        FROM deals d
        INNER JOIN pipelines pip ON d.pipeline_id = pip.id
        INNER JOIN teams t ON pip.team_id = t.id
        INNER JOIN team_members tm ON t.id = tm.team_id
        WHERE tm.user_id = :currentUserId
        AND (d.is_deleted = false OR d.is_deleted IS NULL)
        AND (pip.is_deleted = false OR pip.is_deleted IS NULL)
        AND d.person_id IS NOT NULL
        AND d.pipeline_id IS NOT NULL
    )
)
ORDER BY p.name ASC
LIMIT 50 OFFSET 0;
```

#### For CATEGORY_MANAGER:
```sql
SELECT 
    p.id,
    p.name,
    p.instagram_id,
    p.phone,
    p.email,
    p.lead_date,
    p.venue,
    p.city,
    p.wedding_date,
    p.organization_id,
    p.owner_id,
    p.category_id,
    p.label_enum,
    p.label_id,
    p.source,
    p.sub_source,
    p.created_at,
    p.updated_at,
    p.is_deleted
FROM persons p
LEFT JOIN organizations o ON p.organization_id = o.id
LEFT JOIN users u ON p.owner_id = u.id
WHERE (
    p.is_deleted IS NULL 
    OR p.is_deleted = false
)
AND (
    LOWER(p.name) LIKE '%strangelybornhuman%'
    OR LOWER(p.instagram_id) LIKE '%strangelybornhuman%'
    OR LOWER(p.phone) LIKE '%strangelybornhuman%'
    OR LOWER(p.email) LIKE '%strangelybornhuman%'
    OR LOWER(o.name) LIKE '%strangelybornhuman%'
    OR LOWER(u.first_name) LIKE '%strangelybornhuman%'
    OR LOWER(u.last_name) LIKE '%strangelybornhuman%'
)
AND (
    -- Condition 1: Person's organization is linked to pipelines in teams managed by SALES users who report to this CATEGORY_MANAGER
    p.organization_id IN (
        SELECT DISTINCT pip.organization_id 
        FROM pipelines pip
        INNER JOIN teams t ON pip.team_id = t.id
        INNER JOIN users sales_user ON t.manager_id = sales_user.id
        WHERE sales_user.manager_id = :currentUserId
        AND sales_user.role_id = (SELECT id FROM roles WHERE name = 'SALES')
        AND (pip.is_deleted = false OR pip.is_deleted IS NULL)
    )
    OR
    -- Condition 2: Person has deals in pipelines in teams managed by SALES users who report to this CATEGORY_MANAGER
    p.id IN (
        SELECT DISTINCT d.person_id
        FROM deals d
        INNER JOIN pipelines pip ON d.pipeline_id = pip.id
        INNER JOIN teams t ON pip.team_id = t.id
        INNER JOIN users sales_user ON t.manager_id = sales_user.id
        WHERE sales_user.manager_id = :currentUserId
        AND sales_user.role_id = (SELECT id FROM roles WHERE name = 'SALES')
        AND (d.is_deleted = false OR d.is_deleted IS NULL)
        AND (pip.is_deleted = false OR pip.is_deleted IS NULL)
        AND d.person_id IS NOT NULL
        AND d.pipeline_id IS NOT NULL
    )
)
ORDER BY p.name ASC
LIMIT 50 OFFSET 0;
```

---

## Query Components Explained

### 1. Base Filter: `notDeleted()`
```sql
WHERE (p.is_deleted IS NULL OR p.is_deleted = false)
```
Excludes soft-deleted persons.

### 2. Search Filter: `search(q)`
```sql
AND (
    LOWER(p.name) LIKE '%strangelybornhuman%'
    OR LOWER(p.instagram_id) LIKE '%strangelybornhuman%'
    OR LOWER(p.phone) LIKE '%strangelybornhuman%'
    OR LOWER(p.email) LIKE '%strangelybornhuman%'
    OR LOWER(o.name) LIKE '%strangelybornhuman%'
    OR LOWER(u.first_name) LIKE '%strangelybornhuman%'
    OR LOWER(u.last_name) LIKE '%strangelybornhuman%'
)
```
Searches across:
- Person name
- Instagram ID
- Phone
- Email
- Organization name
- Owner first name
- Owner last name

### 3. Pagination
```sql
LIMIT 50 OFFSET 0
```
- `LIMIT 50` = page size
- `OFFSET 0` = page 0 (first page)

### 4. Sorting
```sql
ORDER BY p.name ASC
```
Sorts by person name in ascending order.

### 5. Role-Based Filtering (Manager Portal Only)

The role-based filtering uses `hasAccessibleOrganizations()` specification which checks:

**Condition 1:** Person's organization is linked to accessible pipelines
```sql
p.organization_id IN (
    SELECT DISTINCT pip.organization_id 
    FROM pipelines pip
    WHERE pip.id IN (:permittedPipelineIds)
)
```

**Condition 2:** Person has deals in accessible pipelines
```sql
p.id IN (
    SELECT DISTINCT d.person_id
    FROM deals d
    WHERE d.pipeline_id IN (:permittedPipelineIds)
    AND (d.is_deleted = false OR d.is_deleted IS NULL)
    AND d.person_id IS NOT NULL
)
```

---

## Count Query (for Pagination Metadata)

Spring Data JPA also generates a count query to get the total number of records:

```sql
SELECT COUNT(DISTINCT p.id)
FROM persons p
LEFT JOIN organizations o ON p.organization_id = o.id
LEFT JOIN users u ON p.owner_id = u.id
WHERE (
    -- Same WHERE conditions as main query
)
```

---

## Performance Considerations

### Recommended Indexes

For optimal performance, ensure these indexes exist:

```sql
-- For search performance
CREATE INDEX idx_persons_name_lower ON persons(LOWER(name));
CREATE INDEX idx_persons_instagram_id_lower ON persons(LOWER(instagram_id));
CREATE INDEX idx_persons_phone_lower ON persons(LOWER(phone));
CREATE INDEX idx_persons_email_lower ON persons(LOWER(email));

-- For soft-delete filtering
CREATE INDEX idx_persons_is_deleted ON persons(is_deleted) WHERE is_deleted = false;

-- For organization joins
CREATE INDEX idx_persons_organization_id ON persons(organization_id);
CREATE INDEX idx_organizations_name_lower ON organizations(LOWER(name));

-- For owner joins
CREATE INDEX idx_persons_owner_id ON persons(owner_id);
CREATE INDEX idx_users_first_name_lower ON users(LOWER(first_name));
CREATE INDEX idx_users_last_name_lower ON users(LOWER(last_name));

-- For role-based filtering (pipeline access)
CREATE INDEX idx_pipelines_organization_id ON pipelines(organization_id);
CREATE INDEX idx_deals_person_id ON deals(person_id);
CREATE INDEX idx_deals_pipeline_id ON deals(pipeline_id);
```

### Query Optimization Tips

1. **Search Performance**: The `LIKE '%term%'` pattern cannot use indexes efficiently. For better performance with large datasets, consider:
   - Full-text search indexes (MySQL FULLTEXT, PostgreSQL tsvector)
   - Search service (Elasticsearch, Algolia)
   - Limiting search to specific fields

2. **Role-Based Filtering**: The subqueries in role-based filtering can be expensive. Consider:
   - Caching permitted pipeline IDs per user
   - Materialized views for pipeline-organization relationships
   - Denormalizing access control data

3. **Pagination**: Always use pagination to limit result sets. The default page size is 25, but can be increased up to a reasonable limit (e.g., 200).

---

## Example Response

```json
{
  "content": [
    {
      "id": 123,
      "name": "Strangely Born Human",
      "instagramId": "@strangelybornhuman",
      "phone": "+1234567890",
      "email": "contact@example.com",
      "leadDate": "2024-01-15",
      "venue": null,
      "city": "Mumbai",
      "eventDate": "2025-12-20",
      "organizationId": 45,
      "organizationName": "Wedding Planner Co",
      "ownerId": 10,
      "ownerDisplayName": "John Doe",
      "categoryId": 1,
      "categoryName": "Makeup",
      "label": "BRIDAL_MAKEUP",
      "source": "INSTAGRAM",
      "createdAt": "2024-01-15T10:30:00Z",
      "updatedAt": "2024-01-20T14:45:00Z"
    }
    // ... 49 more records
  ],
  "pageable": {
    "pageNumber": 0,
    "pageSize": 50,
    "sort": {
      "sorted": true,
      "unsorted": false,
      "empty": false
    }
  },
  "totalElements": 1250,
  "totalPages": 25,
  "last": false,
  "first": true,
  "size": 50,
  "number": 0,
  "numberOfElements": 50,
  "empty": false
}
```

---

## Notes

1. **Case-Insensitive Search**: All search operations use `LOWER()` function for case-insensitive matching.

2. **Soft Delete Handling**: The query always excludes soft-deleted persons (`is_deleted = true`).

3. **Role-Based Access**: For non-admin users, the query automatically applies role-based filtering based on:
   - User's role (ADMIN, CATEGORY_MANAGER, SALES, PRESALES)
   - User's team memberships
   - User's manager hierarchy

4. **Pagination**: Spring Data JPA handles pagination automatically. The `page` parameter is 0-based (0 = first page).

5. **Sorting**: The `sort` parameter format is `field,direction` (e.g., `name,asc` or `createdAt,desc`). Multiple sorts can be specified.

