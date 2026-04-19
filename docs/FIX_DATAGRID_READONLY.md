# Fix: “Column is read-only: No corresponding table column” (IntelliJ / DataGrip / Cursor DB tools)

This message is **not produced by Spring Boot or MySQL permissions**. It comes from the **database plugin’s grid editor**. It appears when the tool **cannot attach an editable cell** to a real row/column pair for generating `UPDATE` statements.

**Renaming the DB column does not fix it** — the limitation is in the IDE, not the column name.

---

## Option A — Fastest fix (recommended): do not use the grid

### 1. Run SQL in a **console** (not the table/grid view)

Replace the numbers with your `categories.id` and `users.id`:

```sql
UPDATE users SET user_managed_category_id = 4 WHERE id = 71;
```

Verify:

```sql
SELECT id, user_managed_category_id, email FROM users WHERE id = 71;
```

See also: `database/quickfix-update-managed-category.sql` in this repo.

### 2. Use the REST API (no DB UI)

- `PATCH /api/users/me/managed-category`  
  Body: `{ "managedCategoryId": <categories.id> }`  
- Use `http/managed-category.http` or `scripts/call-managed-category-api.ps1`

---

## Option B — Try to make the grid editable

Try **in order**:

1. **`SELECT` must include the primary key**  
   Wrong:
   ```sql
   SELECT email, user_managed_category_id FROM users;
   ```
   Right:
   ```sql
   SELECT id, email, user_managed_category_id FROM users;
   ```
   Use `database/users-editable-grid.query.sql` as a template.

2. **Refresh metadata**  
   Database tool → right‑click your data source → **Synchronize** (or disconnect and reconnect).

3. **Append to your JDBC URL** (DataGrip / IntelliJ data source settings, no line breaks):  
   Contents of `database/datagrip-jdbc-url-suffix.txt` in this repo.

4. **Connection must not be read-only**  
   Check data source options / “read only”.

5. **Open `users` from the database tree**  
   Use the table/data view from the tree, not an old result tab from a complex query (joins, aliases, etc.).

6. **Invalidate caches**  
   **Help → Invalidate Caches → Invalidate and Restart**.

---

## Backend note

The API writes `user_managed_category_id` with normal SQL. If `PATCH …/managed-category` succeeds, the database **is** updated even when the IDE grid still shows read-only.
