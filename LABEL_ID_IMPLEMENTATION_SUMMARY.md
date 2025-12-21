# Label ID Implementation Summary

## Current Database Structure

### Persons Table
- ✅ `label_id` (bigint, FK to labels.id) - **This is used to store the label ID**
- `label` (enum) - Old enum column (can be ignored/removed)
- `label_enum` (enum) - Legacy enum for backward compatibility (read-only)

### Deals Table  
- ✅ `label_id` (bigint, FK to labels.id) - **This is used to store the label ID**
- `label` (enum) - Old enum column (should be renamed to `label_enum`)
- `label_enum` (enum) - Legacy enum for backward compatibility (read-only)

## Entity Mapping (Already Correct)

### Deal Entity
```java
@ManyToOne(fetch = FetchType.LAZY)
@JoinColumn(name = "label_id", nullable = true)
private Label label; // Stores label ID from labels table
```

### Person Entity
```java
@ManyToOne(fetch = FetchType.LAZY)
@JoinColumn(name = "label_id", nullable = true)
private Label label; // Stores label ID from labels table
```

## How It Works

1. **Creating a Deal/Person with Label:**
   ```json
   POST /api/deals
   {
     "name": "Wedding Photography",
     "labelId": 3  // This sets label_id = 3 in the deals table
   }
   ```

2. **Database Storage:**
   - The `label_id` column in `deals`/`persons` table stores the ID from `labels` table
   - Example: If `labelId: 3` is sent, then `label_id = 3` is stored in the row

3. **Reading a Deal/Person:**
   ```json
   GET /api/deals/1
   {
     "id": 1,
     "name": "Wedding Photography",
     "label": {
       "id": 3,
       "name": "Hot Lead",
       "color": "#FF5733"
     },
     "labelId": 3
   }
   ```

## Migration Status

✅ **Already Done:**
- `label_id` columns exist in both `deals` and `persons` tables
- Foreign key constraints are set up
- Entity mappings are correct

⚠️ **Optional Cleanup:**
- The old `label` enum column in `deals` table can be renamed to `label_enum` for consistency
- The old `label` enum column in `persons` table can be removed (since `label_enum` already exists)

## API Usage

### Create Deal with Label
```javascript
POST /api/deals
{
  "name": "Wedding Photography",
  "value": 50000,
  "labelId": 3  // Single label ID from labels table
}
```

### Update Deal Label
```javascript
PUT /api/deals/1
{
  "labelId": 5  // Change to different label
}
```

### Remove Label
```javascript
PUT /api/deals/1
{
  "labelId": null  // Remove label
}
```

### Create Person with Label
```javascript
POST /api/persons
{
  "name": "John Doe",
  "labelId": 2  // Single label ID from labels table
}
```

## Frontend Changes Required

1. **Change from array to single value:**
   - Before: `labelIds: [1, 2, 3]`
   - After: `labelId: 3`

2. **Change UI from multi-select to single select:**
   ```jsx
   // Before: Multi-select
   <Select isMulti ... />
   
   // After: Single select
   <Select ... />
   ```

3. **Update state:**
   ```javascript
   // Before
   const [labelIds, setLabelIds] = useState([]);
   
   // After
   const [labelId, setLabelId] = useState(null);
   ```

## Database Column Reference

| Column | Type | Purpose | Status |
|--------|------|---------|--------|
| `label_id` | bigint FK | Stores label ID from labels table | ✅ Active |
| `label` | enum | Old enum (deals table) | ⚠️ Can be removed |
| `label_enum` | enum | Legacy enum (read-only) | 📖 Read-only |

## Summary

✅ **The implementation is correct!** The `label_id` column in both tables is already being used to store the label ID from the `labels` table. The entity mappings are correct, and the API accepts `labelId` in requests.

The only thing needed is to update the frontend to use `labelId` (single value) instead of `labelIds` (array).

