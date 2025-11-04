# Manual Steps to Update Role Names

If you're getting the "Data truncated" error, follow these steps:

## Step 1: Check the Column Type

First, check what type the `name` column is:

```sql
DESCRIBE roles;
```

Or:

```sql
SHOW COLUMNS FROM roles LIKE 'name';
```

## Step 2A: If Column is ENUM

If the column is an ENUM type, run the main migration script:
`update_roles_managerto_sales.sql`

This script will:
1. Modify the ENUM to include new values
2. Update the data
3. Clean up the ENUM definition

## Step 2B: If Column is VARCHAR

If the column is VARCHAR, run the alternative script:
`update_roles_managerto_sales_alternative.sql`

This script just updates the data directly.

## Step 3: Verify

After running the migration, verify with:

```sql
SELECT id, name, description FROM roles ORDER BY id;
```

You should see:
- ADMIN
- CATEGORY_MANAGER
- SALES (formerly MANAGER)
- PRESALES (formerly SALESREP)

## Alternative: Convert ENUM to VARCHAR

If you want to convert the ENUM to VARCHAR (more flexible for future changes):

```sql
-- Step 1: Convert ENUM to VARCHAR
ALTER TABLE roles 
MODIFY COLUMN name VARCHAR(50) NOT NULL;

-- Step 2: Update the data
UPDATE roles 
SET name = 'SALES' 
WHERE name = 'MANAGER';

UPDATE roles 
SET name = 'PRESALES' 
WHERE name = 'SALESREP';

-- Step 3: Add unique constraint back (if needed)
ALTER TABLE roles 
ADD UNIQUE KEY unique_name (name);
```

