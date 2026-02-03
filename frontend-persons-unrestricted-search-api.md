# Frontend Implementation: Unrestricted Person Search API

## Overview
This document describes the new unrestricted person search endpoint (`/api/persons/search`) that allows users to search for any person without role-based access restrictions. This is intended for use cases like "create deal" where users need to find and select any person regardless of their role-based access control.

## API Endpoint

### Search Persons (Unrestricted)

**Endpoint:** `GET /api/persons/search`

**Description:** Search persons without role-based access restrictions. This endpoint does NOT apply role-based organization or pipeline filtering, allowing all users to search for any person in the system. Still respects soft-delete filtering and all user-provided filters.

**Authentication:** Required (Bearer token)

**Query Parameters:**
- `q` (string, optional): Search term - searches in name, instagram, phone, email, organization name, owner name
- `label` (string, optional): Person label filter (e.g., `BRIDAL_MAKEUP`, `PARTY_MAKEUP`)
- `source` (string, optional): Person source filter
- `dealSource` (string, optional): Deal source filter
- `organizationId` (string, optional): Comma-separated organization IDs (e.g., `1,2,3`)
- `ownerId` (string, optional): Comma-separated owner IDs (e.g., `1,2,3`)
- `categoryId` (string, optional): Comma-separated category IDs (e.g., `1,2,3`)
- `leadFrom` (date, optional): Start date for lead date range (format: `dd/MM/yyyy`)
- `leadTo` (date, optional): End date for lead date range (format: `dd/MM/yyyy`)
- `page` (integer, optional): Page number (0-based, default: 0)
- `size` (integer, optional): Page size (default: 50)
- `sort` (string, optional): Sort field and direction (default: `name,asc`)

**Request Example:**
```javascript
GET /api/persons/search?q=strangelybornhuman&page=0&size=50&sort=name,asc
Authorization: Bearer <token>
```

**Response Format:**
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
    // ... more persons
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

## Key Differences from `/api/persons`

| Feature | `/api/persons` | `/api/persons/search` |
|---------|----------------|----------------------|
| **Role-based filtering** | ✅ Applied (respects user's role and access) | ❌ NOT applied (unrestricted) |
| **Organization filtering** | ✅ Filtered by user's accessible organizations | ❌ Shows all organizations |
| **Pipeline filtering** | ✅ Filtered by user's accessible pipelines | ❌ Shows all pipelines |
| **Soft-delete filtering** | ✅ Applied | ✅ Applied |
| **User-provided filters** | ✅ Applied | ✅ Applied |
| **Use case** | Person list page (with access control) | Create deal, select person (unrestricted) |

---

## Frontend Integration

### Step 1: Create API Service Function

Add this function to your API service file (e.g., `api/persons.js`):

```javascript
/**
 * Search persons without role-based restrictions (unrestricted search)
 * Use this for "create deal" and similar scenarios where users need to find any person
 * @param {string} query - Search term
 * @param {Object} filters - Additional filters (optional)
 * @param {Object} pagination - Pagination options (optional)
 * @returns {Promise<Object>} Paginated response with persons
 */
export async function searchPersonsUnrestricted(query, filters = {}, pagination = {}) {
  const params = new URLSearchParams();
  
  // Add search query
  if (query) {
    params.append('q', query);
  }
  
  // Add filters
  if (filters.label) {
    params.append('label', filters.label);
  }
  if (filters.organizationId) {
    params.append('organizationId', Array.isArray(filters.organizationId) 
      ? filters.organizationId.join(',') 
      : filters.organizationId);
  }
  if (filters.ownerId) {
    params.append('ownerId', Array.isArray(filters.ownerId) 
      ? filters.ownerId.join(',') 
      : filters.ownerId);
  }
  if (filters.categoryId) {
    params.append('categoryId', Array.isArray(filters.categoryId) 
      ? filters.categoryId.join(',') 
      : filters.categoryId);
  }
  if (filters.leadFrom) {
    params.append('leadFrom', filters.leadFrom);
  }
  if (filters.leadTo) {
    params.append('leadTo', filters.leadTo);
  }
  
  // Add pagination
  params.append('page', pagination.page || 0);
  params.append('size', pagination.size || 50);
  if (pagination.sort) {
    params.append('sort', pagination.sort);
  }
  
  const url = `${API_BASE_URL}/persons/search?${params.toString()}`;
  
  const response = await authenticatedFetch(url, {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json',
    },
  });

  if (!response.ok) {
    if (response.status === 401) {
      throw new Error('Unauthorized. Please login again.');
    }
    throw new Error(`Failed to search persons: ${response.statusText}`);
  }

  return await response.json();
}
```

### Step 2: Use in Create Deal Page

Example implementation for person picker in create deal page:

```javascript
// Example: create-deal-page.js

let personSearchResults = [];
let personSearchLoading = false;
let personSearchQuery = '';

/**
 * Search for persons when user types in person picker
 * @param {string} searchTerm - Search query
 */
async function searchPersonsForDeal(searchTerm) {
  personSearchQuery = searchTerm;
  personSearchLoading = true;
  
  try {
    // Use unrestricted search endpoint
    const response = await searchPersonsUnrestricted(searchTerm, {}, {
      page: 0,
      size: 50,
      sort: 'name,asc'
    });
    
    personSearchResults = response.content || [];
    updatePersonDropdown(personSearchResults);
  } catch (error) {
    console.error('Error searching persons:', error);
    showNotification(error.message || 'Failed to search persons', 'error');
    personSearchResults = [];
    updatePersonDropdown([]);
  } finally {
    personSearchLoading = false;
  }
}

/**
 * Update person dropdown with search results
 * @param {Array} persons - Array of person objects
 */
function updatePersonDropdown(persons) {
  const personSelect = document.getElementById('person-select');
  if (!personSelect) return;

  // Clear existing options except "Select Person"
  personSelect.innerHTML = '<option value="">Select Person</option>';

  if (persons.length === 0) {
    if (personSearchQuery) {
      const option = document.createElement('option');
      option.value = '';
      option.textContent = 'No persons found';
      option.disabled = true;
      personSelect.appendChild(option);
    }
    return;
  }

  // Add persons to dropdown
  persons.forEach(person => {
    const option = document.createElement('option');
    option.value = person.id;
    option.textContent = person.name;
    if (person.organizationName) {
      option.textContent += ` (${person.organizationName})`;
    }
    personSelect.appendChild(option);
  });
}

// Attach to search input
const personSearchInput = document.getElementById('person-search');
if (personSearchInput) {
  let searchTimeout;
  personSearchInput.addEventListener('input', (e) => {
    const query = e.target.value.trim();
    
    // Debounce search
    clearTimeout(searchTimeout);
    searchTimeout = setTimeout(() => {
      if (query.length >= 2) {
        searchPersonsForDeal(query);
      } else {
        personSearchResults = [];
        updatePersonDropdown([]);
      }
    }, 300); // Wait 300ms after user stops typing
  });
}
```

### Step 3: React Component Example

If using React, here's a complete example:

```jsx
import { useState, useEffect, useCallback } from 'react';
import { searchPersonsUnrestricted } from '../api/persons';
import { useDebounce } from '../hooks/useDebounce';

function PersonPicker({ onPersonSelect, selectedPersonId }) {
  const [searchQuery, setSearchQuery] = useState('');
  const [persons, setPersons] = useState([]);
  const [loading, setLoading] = useState(false);
  const [showDropdown, setShowDropdown] = useState(false);
  
  const debouncedQuery = useDebounce(searchQuery, 300);

  useEffect(() => {
    if (debouncedQuery.length >= 2) {
      searchPersons();
    } else {
      setPersons([]);
      setShowDropdown(false);
    }
  }, [debouncedQuery]);

  const searchPersons = async () => {
    try {
      setLoading(true);
      const response = await searchPersonsUnrestricted(debouncedQuery, {}, {
        page: 0,
        size: 50,
        sort: 'name,asc'
      });
      setPersons(response.content || []);
      setShowDropdown(true);
    } catch (error) {
      console.error('Error searching persons:', error);
      alert(error.message || 'Failed to search persons');
      setPersons([]);
    } finally {
      setLoading(false);
    }
  };

  const handlePersonSelect = (person) => {
    onPersonSelect(person);
    setShowDropdown(false);
    setSearchQuery(person.name);
  };

  return (
    <div className="person-picker">
      <input
        type="text"
        value={searchQuery}
        onChange={(e) => setSearchQuery(e.target.value)}
        onFocus={() => persons.length > 0 && setShowDropdown(true)}
        placeholder="Search for person..."
        className="form-control"
      />
      {loading && <div className="spinner">Loading...</div>}
      {showDropdown && persons.length > 0 && (
        <div className="dropdown-menu show">
          {persons.map((person) => (
            <div
              key={person.id}
              className="dropdown-item"
              onClick={() => handlePersonSelect(person)}
            >
              <div className="person-name">{person.name}</div>
              {person.organizationName && (
                <div className="person-org text-muted">{person.organizationName}</div>
              )}
            </div>
          ))}
        </div>
      )}
      {showDropdown && !loading && persons.length === 0 && debouncedQuery.length >= 2 && (
        <div className="dropdown-menu show">
          <div className="dropdown-item text-muted">No persons found</div>
        </div>
      )}
    </div>
  );
}
```

---

## When to Use Each Endpoint

### Use `/api/persons` (with restrictions) when:
- ✅ Displaying person list page
- ✅ Showing persons in dashboard
- ✅ Filtering persons by user's accessible data
- ✅ Any scenario where role-based access control is important

### Use `/api/persons/search` (unrestricted) when:
- ✅ Creating a new deal and need to select a person
- ✅ Creating a new activity and need to select a person
- ✅ Any scenario where user needs to find ANY person in the system
- ✅ Person picker/autocomplete components

---

## Security Considerations

⚠️ **Important Notes:**

1. **Authentication Required**: The unrestricted endpoint still requires authentication. Only authenticated users can access it.

2. **No Data Modification**: This endpoint is read-only. It only allows searching, not creating or modifying persons.

3. **Soft-Delete Respect**: Deleted persons are still excluded from results.

4. **Use Case Specific**: This endpoint is designed for specific use cases like "create deal". Don't use it for general person listing where access control is important.

5. **Audit Trail**: All searches are logged through authentication, so there's still an audit trail of who searched for what.

---

## Performance Considerations

1. **Pagination**: Always use pagination. Default page size is 50, but you can adjust based on your needs.

2. **Debouncing**: Implement debouncing for search inputs to avoid excessive API calls.

3. **Minimum Query Length**: Consider requiring at least 2-3 characters before searching to reduce unnecessary queries.

4. **Caching**: Consider caching search results for common queries to improve performance.

---

## Testing Checklist

- [ ] Search works with query parameter
- [ ] Search returns all persons (not filtered by role)
- [ ] Pagination works correctly
- [ ] Sorting works correctly
- [ ] Filters (label, organization, owner, category) work correctly
- [ ] Soft-deleted persons are excluded
- [ ] Loading states display correctly
- [ ] Error handling works for network failures
- [ ] Empty state shows when no results found
- [ ] Works in create deal page
- [ ] Works in create activity page
- [ ] Authentication is required

---

## Migration Guide

If you're currently using `/api/persons` in places where you need unrestricted access:

### Before:
```javascript
// In create deal page
const response = await fetch(`${API_BASE_URL}/persons?q=${query}`);
```

### After:
```javascript
// In create deal page - use unrestricted endpoint
const response = await searchPersonsUnrestricted(query);
```

### Keep using `/api/persons` for:
- Person list pages
- Dashboard views
- Any place where role-based filtering is desired

---

## Example: Complete Integration

```javascript
// api/persons.js
export async function searchPersonsUnrestricted(query, filters = {}, pagination = {}) {
  // ... implementation from Step 1
}

// create-deal.js
import { searchPersonsUnrestricted } from './api/persons';

class CreateDealPage {
  async init() {
    this.setupPersonSearch();
  }

  setupPersonSearch() {
    const personInput = document.getElementById('person-search');
    let debounceTimer;

    personInput.addEventListener('input', (e) => {
      const query = e.target.value.trim();
      clearTimeout(debounceTimer);

      if (query.length < 2) {
        this.clearPersonResults();
        return;
      }

      debounceTimer = setTimeout(async () => {
        await this.searchPersons(query);
      }, 300);
    });
  }

  async searchPersons(query) {
    try {
      this.showPersonLoading();
      const response = await searchPersonsUnrestricted(query, {}, {
        page: 0,
        size: 50,
        sort: 'name,asc'
      });
      this.displayPersonResults(response.content);
    } catch (error) {
      this.showPersonError(error.message);
    } finally {
      this.hidePersonLoading();
    }
  }

  displayPersonResults(persons) {
    const dropdown = document.getElementById('person-dropdown');
    dropdown.innerHTML = '';

    if (persons.length === 0) {
      dropdown.innerHTML = '<div class="no-results">No persons found</div>';
      return;
    }

    persons.forEach(person => {
      const item = document.createElement('div');
      item.className = 'person-item';
      item.innerHTML = `
        <div class="person-name">${person.name}</div>
        ${person.organizationName ? `<div class="person-org">${person.organizationName}</div>` : ''}
      `;
      item.onclick = () => this.selectPerson(person);
      dropdown.appendChild(item);
    });
  }

  selectPerson(person) {
    document.getElementById('person-id').value = person.id;
    document.getElementById('person-search').value = person.name;
    document.getElementById('person-dropdown').innerHTML = '';
  }
}
```

---

## Notes

- The endpoint uses the same response format as `/api/persons` for consistency
- Default sort is `name,asc` (alphabetical by name)
- Default page size is 50 (can be adjusted)
- Search is case-insensitive
- Search looks in: name, instagram, phone, email, organization name, owner name
- All filters are optional - you can search with just a query string

