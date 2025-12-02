# Person Category Integration Guide

This guide explains how to integrate the new `category_id` field in the Person entity, replacing the old `category` column.

## Overview

The `persons` table now uses `category_id` (foreign key to `categories` table) instead of a direct `category` column. This allows for better data normalization and easier category management.

## Database Migration

**IMPORTANT:** Before deploying, you need to run a database migration:

```sql
-- Step 1: Add the new category_id column (nullable initially)
ALTER TABLE persons ADD COLUMN category_id BIGINT NULL;

-- Step 2: Add foreign key constraint
ALTER TABLE persons 
ADD CONSTRAINT fk_person_category 
FOREIGN KEY (category_id) REFERENCES categories(id) ON DELETE SET NULL;

-- Step 3: If you have existing data, you may need to map old category values to category_id
-- (This step depends on your existing data structure)
-- Example:
-- UPDATE persons p
-- INNER JOIN categories c ON c.name = p.category
-- SET p.category_id = c.id
-- WHERE p.category IS NOT NULL;

-- Step 4: Remove the old category column (if it exists)
-- ALTER TABLE persons DROP COLUMN category;
```

## API Changes

### 1. Get Categories List

**Endpoint:** `GET /api/persons/categories`

**Description:** Returns a list of all available categories from the `categories` table.

**Response:**
```json
{
  "success": true,
  "message": "Categories retrieved successfully",
  "data": [
    {
      "id": 1,
      "name": "Photography"
    },
    {
      "id": 2,
      "name": "Makeup"
    },
    {
      "id": 3,
      "name": "Planning & Decor"
    }
  ]
}
```

### 2. Create Person

**Endpoint:** `POST /api/persons`

**Request Body:**
```json
{
  "name": "John Doe",
  "instagramId": "@johndoe",
  "phone": "+1234567890",
  "email": "john@example.com",
  "leadDate": "2024-01-15",
  "organizationId": 1,
  "ownerId": 2,
  "categoryId": 1,
  "label": "BRIDAL_MAKEUP",
  "source": "Direct",
  "subSource": "Instagram"
}
```

**Changes:**
- ✅ **Added:** `categoryId` (Long, optional) - The ID of the category from the categories table
- ❌ **Removed:** `category` (String) - No longer accepted

**Response:**
```json
{
  "id": 123,
  "name": "John Doe",
  "instagramId": "@johndoe",
  "phone": "+1234567890",
  "email": "john@example.com",
  "leadDate": "2024-01-15",
  "organizationId": 1,
  "organizationName": "Example Organization",
  "ownerId": 2,
  "ownerDisplayName": "Jane Smith",
  "ownerEmail": "jane@example.com",
  "categoryId": 1,
  "categoryName": "Photography",
  "label": "BRIDAL_MAKEUP",
  "source": "Direct",
  "subSource": "Instagram",
  "createdAt": "2024-01-15T10:30:00Z",
  "updatedAt": "2024-01-15T10:30:00Z"
}
```

**Response Changes:**
- ✅ **Added:** `categoryId` (Long) - The category ID
- ✅ **Added:** `categoryName` (String, read-only) - The category name for display

### 3. Update Person

**Endpoint:** `PUT /api/persons/{id}`

**Request Body:**
```json
{
  "name": "John Doe Updated",
  "categoryId": 2,
  "source": "Reference"
}
```

**Changes:**
- ✅ **Added:** `categoryId` (Long, optional) - Can be updated or set to `null` to remove category
- ❌ **Removed:** `category` (String) - No longer accepted

**Note:** To remove a category, send `categoryId: null` in the update request.

### 4. Get Person

**Endpoint:** `GET /api/persons/{id}`

**Response:** Same as Create Person response above, includes `categoryId` and `categoryName`.

### 5. List Persons

**Endpoint:** `GET /api/persons`

**Response:** Array of person objects, each includes `categoryId` and `categoryName`.

## Frontend Integration Steps

### Step 1: Fetch Categories on Page Load

```javascript
// Fetch categories when the person form/page loads
async function fetchCategories() {
  try {
    const response = await fetch('/api/persons/categories', {
      headers: {
        'Authorization': `Bearer ${token}`
      }
    });
    const result = await response.json();
    if (result.success) {
      return result.data; // Array of {id, name}
    }
  } catch (error) {
    console.error('Error fetching categories:', error);
  }
  return [];
}
```

### Step 2: Update Person Form

**Before (Old):**
```javascript
// Old way - using category string
const formData = {
  name: "John Doe",
  category: "Photography", // ❌ Remove this
  // ... other fields
};
```

**After (New):**
```javascript
// New way - using categoryId
const formData = {
  name: "John Doe",
  categoryId: 1, // ✅ Use category ID instead
  // ... other fields
};
```

### Step 3: Update Category Dropdown

```javascript
// Example React component
function PersonForm() {
  const [categories, setCategories] = useState([]);
  const [selectedCategoryId, setSelectedCategoryId] = useState(null);

  useEffect(() => {
    // Fetch categories on mount
    fetchCategories().then(setCategories);
  }, []);

  return (
    <form>
      {/* Other form fields */}
      
      <select 
        value={selectedCategoryId || ''} 
        onChange={(e) => setSelectedCategoryId(e.target.value ? parseInt(e.target.value) : null)}
      >
        <option value="">Select Category (Optional)</option>
        {categories.map(cat => (
          <option key={cat.id} value={cat.id}>
            {cat.name}
          </option>
        ))}
      </select>
      
      {/* Submit with categoryId */}
      <button onClick={() => submitPerson({ categoryId: selectedCategoryId })}>
        Submit
      </button>
    </form>
  );
}
```

### Step 4: Display Category in Person List/Details

```javascript
// When displaying person data
function PersonCard({ person }) {
  return (
    <div>
      <h3>{person.name}</h3>
      {person.categoryName && (
        <p>Category: {person.categoryName}</p>
      )}
      {/* Other person details */}
    </div>
  );
}
```

## Validation Rules

1. **categoryId is optional** - A person can be created/updated without a category
2. **categoryId must be valid** - If provided, it must exist in the `categories` table
3. **Error handling** - If an invalid `categoryId` is provided, the API will return:
   ```json
   {
     "success": false,
     "message": "Category not found with id {categoryId}",
     "data": null
   }
   ```

## Migration Checklist

- [ ] Run database migration SQL script
- [ ] Update frontend to fetch categories from `/api/persons/categories`
- [ ] Replace `category` field with `categoryId` in person forms
- [ ] Update person create/update API calls to use `categoryId`
- [ ] Update person display components to show `categoryName`
- [ ] Test creating a person with a category
- [ ] Test creating a person without a category
- [ ] Test updating a person's category
- [ ] Test removing a person's category (set `categoryId: null`)
- [ ] Verify category dropdown is populated correctly

## Example API Calls

### Create Person with Category
```bash
curl -X POST http://localhost:8080/api/persons \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -d '{
    "name": "Jane Doe",
    "email": "jane@example.com",
    "categoryId": 1,
    "source": "Direct",
    "subSource": "Instagram"
  }'
```

### Update Person Category
```bash
curl -X PUT http://localhost:8080/api/persons/123 \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -d '{
    "categoryId": 2
  }'
```

### Remove Person Category
```bash
curl -X PUT http://localhost:8080/api/persons/123 \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -d '{
    "categoryId": null
  }'
```

## Notes

- The `categoryName` field in responses is **read-only** and is automatically populated from the `categories` table
- Categories are managed separately (likely through a categories management interface)
- The relationship is optional - persons can exist without a category
- If a category is deleted from the `categories` table, the `category_id` in `persons` will be set to `NULL` (due to `ON DELETE SET NULL`)

