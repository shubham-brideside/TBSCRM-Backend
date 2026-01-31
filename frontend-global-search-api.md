# Global Search API Documentation

## Overview

The Global Search API allows you to search across both **persons** and **deals** in a single API call. It searches by name, instagram_id, and phone_number across both entity types.

## Endpoint

```
GET /api/search?q={query}&limit={limit}
```

## Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `q` | String | Yes | - | Search query - searches in name, instagram_id, and phone_number |
| `limit` | Integer | No | 10 | Maximum number of results per entity type (persons and deals) |

## What Gets Searched

### Persons
- Person name
- Instagram ID (`instagramId`)
- Phone number (`phone`)
- Email
- Organization name (via JOIN)
- Owner name (via JOIN)

### Deals
- Deal name
- Venue
- Phone number (`phoneNumber`)
- Person name (via JOIN with related person)
- Person Instagram ID (via JOIN with related person)
- Person phone (via JOIN with related person)
- Organization name (via JOIN)

## Response Format

```json
{
  "success": true,
  "message": "Found 5 persons and 3 deals",
  "data": {
    "persons": [
      {
        "id": 101,
        "name": "John Doe",
        "instagramId": "@johndoe",
        "phone": "+1234567890",
        "email": "john@example.com",
        "organizationId": 12,
        "organizationName": "Example Org",
        "ownerId": 42,
        "ownerDisplayName": "Jane Smith",
        ...
      }
    ],
    "deals": [
      {
        "id": 215,
        "name": "Wedding Package - Doe",
        "value": 125000,
        "personId": 101,
        "personName": "John Doe",
        "phoneNumber": "+1234567890",
        "venue": "The Taj Palace",
        ...
      }
    ],
    "personsCount": 5,
    "dealsCount": 3,
    "totalCount": 8
  }
}
```

## Response Fields

### GlobalSearchResponse

| Field | Type | Description |
|-------|------|-------------|
| `persons` | Array<PersonDTO> | List of matching persons |
| `deals` | Array<DealResponse> | List of matching deals |
| `personsCount` | Integer | Number of persons found |
| `dealsCount` | Integer | Number of deals found |
| `totalCount` | Integer | Total number of results (persons + deals) |

### PersonDTO Fields

See [Persons API Documentation](./frontend-persons-api.md) for full PersonDTO structure.

Key fields:
- `id`, `name`, `instagramId`, `phone`, `email`
- `organizationId`, `organizationName`
- `ownerId`, `ownerDisplayName`

### DealResponse Fields

See [Deals API Documentation](./frontend-deals-api.md) for full DealResponse structure.

Key fields:
- `id`, `name`, `value`, `phoneNumber`, `venue`
- `personId`, `personName`
- `organizationId`, `organizationName`
- `pipelineId`, `stageId`, `status`

## Example Requests

### Basic Search

```bash
GET /api/search?q=john
```

**Response:**
```json
{
  "success": true,
  "message": "Found 2 persons and 1 deals",
  "data": {
    "persons": [...],
    "deals": [...],
    "personsCount": 2,
    "dealsCount": 1,
    "totalCount": 3
  }
}
```

### Search with Custom Limit

```bash
GET /api/search?q=john&limit=20
```

Returns up to 20 persons and 20 deals (40 total results max).

### Search by Instagram ID

```bash
GET /api/search?q=@johndoe
```

### Search by Phone Number

```bash
GET /api/search?q=+1234567890
```

## Error Responses

### Missing Query Parameter

```json
{
  "success": false,
  "message": "Search query 'q' parameter is required and cannot be empty",
  "data": null
}
```

**Status Code:** `400 Bad Request`

### Empty Query

```json
{
  "success": false,
  "message": "Search query 'q' parameter is required and cannot be empty",
  "data": null
}
```

**Status Code:** `400 Bad Request`

## Frontend Integration Example

### TypeScript/React Example

```typescript
interface GlobalSearchResult {
  persons: PersonDTO[];
  deals: DealResponse[];
  personsCount: number;
  dealsCount: number;
  totalCount: number;
}

async function globalSearch(query: string, limit: number = 10): Promise<GlobalSearchResult> {
  const response = await fetch(`/api/search?q=${encodeURIComponent(query)}&limit=${limit}`, {
    headers: {
      'Authorization': `Bearer ${token}`,
      'Content-Type': 'application/json'
    }
  });
  
  if (!response.ok) {
    throw new Error('Search failed');
  }
  
  const data = await response.json();
  return data.data;
}

// Usage
const results = await globalSearch('john', 10);
console.log(`Found ${results.personsCount} persons and ${results.dealsCount} deals`);
```

### JavaScript Example

```javascript
async function globalSearch(query, limit = 10) {
  const response = await fetch(`/api/search?q=${encodeURIComponent(query)}&limit=${limit}`);
  const data = await response.json();
  return data.data;
}

// Usage
const results = await globalSearch('john');
console.log(`Found ${results.personsCount} persons and ${results.dealsCount} deals`);
```

## Search Behavior

1. **Case-Insensitive:** All searches are case-insensitive
2. **Partial Matching:** Uses `LIKE '%query%'` pattern matching
3. **Multiple Fields:** Searches across all relevant fields simultaneously
4. **OR Logic:** Results match if ANY field contains the query
5. **Pagination:** Uses `limit` parameter to control result count per entity type

## Performance Considerations

- **Default Limit:** 10 results per entity type (20 total max)
- **Recommended Limit:** 10-20 for optimal performance
- **Large Limits:** Avoid setting limit > 50 as it may impact performance
- **Empty Results:** Returns empty arrays if no matches found

## Comparison with Separate Endpoints

### Before (Separate Calls)
```typescript
// Required 2 API calls
const persons = await fetch('/api/persons?q=john');
const deals = await fetch('/api/deals?search=john');
```

### After (Global Search)
```typescript
// Single API call
const results = await fetch('/api/search?q=john');
```

**Benefits:**
- ✅ Single API call instead of two
- ✅ Consistent search across both entities
- ✅ Unified response format
- ✅ Better performance (parallel queries on backend)

## Notes

1. **Deal Search Enhancement:** The deals search now also includes:
   - `phoneNumber` (from deals table)
   - `instagramId` (from related person)
   
   This was added as part of the global search implementation.

2. **Role-Based Filtering:** The search respects user permissions and role-based access control (same as individual endpoints).

3. **Soft-Deleted Records:** Soft-deleted persons and deals are excluded from search results.

## Related Endpoints

- `GET /api/persons?q={query}` - Search persons only
- `GET /api/deals?search={query}` - Search deals only

Use the global search endpoint when you need results from both entities in a single call.

