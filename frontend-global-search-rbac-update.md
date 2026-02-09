# Global Search API - RBAC Update Guide

## 📋 Overview

The Global Search API (`/api/search`) has been updated with **Role-Based Access Control (RBAC)** to ensure users only see persons and deals they have access to. This is a **security enhancement** that filters results based on the logged-in user's role and organization hierarchy.

## ⚠️ Breaking Change: Authentication Now Required

**CRITICAL:** The global search endpoint now **requires authentication**. All requests must include a valid Bearer token in the Authorization header.

### Before (No Authentication)
```javascript
// ❌ This will now return 401 Unauthorized
const response = await fetch('/api/search?q=john');
```

### After (Authentication Required)
```javascript
// ✅ Must include Bearer token
const token = localStorage.getItem('token');
const response = await fetch('/api/search?q=john', {
  headers: {
    'Authorization': `Bearer ${token}`
  }
});
```

## 🔄 What Changed

### Backend Changes (Automatic)
- ✅ Authentication is now required
- ✅ Results are automatically filtered by user role
- ✅ API response structure remains **exactly the same**

### What You Need to Do
1. **Add authentication** to all global search API calls
2. **Handle 401/403 errors** appropriately
3. **Test with different user roles** to verify filtering

## 🎯 Role-Based Filtering Behavior

Results are automatically filtered based on the logged-in user's role:

| Role | What They See |
|------|---------------|
| **ADMIN** | All persons and deals (no filtering) |
| **CATEGORY_MANAGER** | Persons/deals for:<br>• Organizations they own<br>• Organizations owned by Sales/Presales directly under them<br>• Organizations owned by Presales under their Sales managers |
| **SALES** | Persons/deals for:<br>• Organizations they own<br>• Organizations owned by Presales under them |
| **PRESALES** | Persons/deals for:<br>• Same as Sales (if they have a Sales manager):<br>  - Organizations owned by their Sales manager<br>  - Organizations owned by all Presales under that Sales manager (including themselves)<br>• If no Sales manager: Only organizations they own |

**Note:** This filtering happens automatically on the backend. You don't need to implement any filtering logic in the frontend.

## 📝 Implementation Steps

### Step 1: Update Your API Service Function

Update your global search service to include authentication:

```typescript
// services/searchService.ts

import { ApiResponse, GlobalSearchResponse } from '../types/search';

const API_BASE_URL = process.env.REACT_APP_API_URL || 'http://localhost:8080';

export async function globalSearch(
  query: string,
  limit: number = 10
): Promise<GlobalSearchResponse> {
  if (!query || query.trim().length === 0) {
    throw new Error('Search query cannot be empty');
  }

  // ✅ Get authentication token
  const token = localStorage.getItem('token');
  if (!token) {
    throw new Error('Authentication required. Please login.');
  }

  const url = new URL(`${API_BASE_URL}/api/search`);
  url.searchParams.append('q', query.trim());
  if (limit) {
    url.searchParams.append('limit', limit.toString());
  }

  const response = await fetch(url.toString(), {
    method: 'GET',
    headers: {
      'Authorization': `Bearer ${token}`,  // ✅ Required
      'Content-Type': 'application/json'
    }
  });

  // ✅ Handle authentication errors
  if (response.status === 401) {
    // Token expired or invalid
    localStorage.removeItem('token');
    localStorage.removeItem('user');
    throw new Error('Session expired. Please login again.');
  }

  if (response.status === 403) {
    throw new Error('Access denied. You do not have permission to perform this action.');
  }

  if (!response.ok) {
    const errorData = await response.json().catch(() => ({ message: 'Request failed' }));
    throw new Error(errorData.message || `HTTP error! status: ${response.status}`);
  }

  const data: ApiResponse<GlobalSearchResponse> = await response.json();
  
  if (!data.success) {
    throw new Error(data.message || 'Search failed');
  }

  return data.data;
}
```

### Step 2: Update Your Search Hook

Add proper error handling for authentication failures:

```typescript
// hooks/useGlobalSearch.ts

import { useState, useEffect, useCallback } from 'react';
import { globalSearch } from '../services/searchService';
import { GlobalSearchResponse } from '../types/search';
import { useNavigate } from 'react-router-dom'; // or your navigation method

interface UseGlobalSearchOptions {
  debounceMs?: number;
  limit?: number;
}

export function useGlobalSearch(options: UseGlobalSearchOptions = {}) {
  const { debounceMs = 300, limit = 10 } = options;
  const navigate = useNavigate();
  
  const [query, setQuery] = useState('');
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [results, setResults] = useState<GlobalSearchResponse | null>(null);

  const search = useCallback(async (searchQuery: string) => {
    if (!searchQuery || searchQuery.trim().length === 0) {
      setResults(null);
      setError(null);
      return;
    }

    setLoading(true);
    setError(null);

    try {
      const searchResults = await globalSearch(searchQuery, limit);
      setResults(searchResults);
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Search failed';
      setError(errorMessage);
      setResults(null);
      
      // ✅ Handle authentication errors
      if (errorMessage.includes('Session expired') || 
          errorMessage.includes('Authentication required')) {
        // Redirect to login
        setTimeout(() => {
          navigate('/login');
        }, 2000); // Give user time to see the error message
      }
    } finally {
      setLoading(false);
    }
  }, [limit, navigate]);

  // Debounce implementation
  useEffect(() => {
    const timer = setTimeout(() => {
      if (query) {
        search(query);
      } else {
        setResults(null);
        setError(null);
      }
    }, debounceMs);

    return () => clearTimeout(timer);
  }, [query, debounceMs, search]);

  const clearSearch = useCallback(() => {
    setQuery('');
    setResults(null);
    setError(null);
  }, []);

  return {
    query,
    setQuery,
    results,
    loading,
    error,
    search,
    clearSearch
  };
}
```

### Step 3: Update Your Search Component

Ensure your search component handles authentication errors gracefully:

```typescript
// components/GlobalSearch.tsx

import React from 'react';
import { useGlobalSearch } from '../hooks/useGlobalSearch';
import { useNavigate } from 'react-router-dom';

export function GlobalSearch() {
  const navigate = useNavigate();
  const { query, results, loading, error, setQuery, clearSearch } = useGlobalSearch({
    limit: 10
  });

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setQuery(e.target.value);
  };

  return (
    <div className="global-search">
      <div className="search-input-container">
        <input
          type="text"
          placeholder="Search persons and deals..."
          value={query}
          onChange={handleInputChange}
          className="search-input"
        />
        {query && (
          <button onClick={clearSearch} className="clear-button">
            ×
          </button>
        )}
      </div>
      
      {loading && (
        <div className="loading-indicator">
          <span>Searching...</span>
        </div>
      )}
      
      {error && (
        <div className="error-message">
          <p>{error}</p>
          {(error.includes('Session expired') || error.includes('Authentication required')) && (
            <button 
              onClick={() => navigate('/login')}
              className="login-button"
            >
              Go to Login
            </button>
          )}
        </div>
      )}
      
      {results && !loading && (
        <div className="search-results">
          {results.personsCount > 0 && (
            <div className="results-section">
              <h3>Persons ({results.personsCount})</h3>
              <ul>
                {results.persons.map(person => (
                  <li key={person.id}>
                    <a href={`/persons/${person.id}`}>
                      {person.name}
                      {person.organizationName && (
                        <span className="org-name"> - {person.organizationName}</span>
                      )}
                    </a>
                  </li>
                ))}
              </ul>
            </div>
          )}
          
          {results.dealsCount > 0 && (
            <div className="results-section">
              <h3>Deals ({results.dealsCount})</h3>
              <ul>
                {results.deals.map(deal => (
                  <li key={deal.id}>
                    <a href={`/deals/${deal.id}`}>
                      {deal.name}
                      {deal.personName && (
                        <span className="person-name"> - {deal.personName}</span>
                      )}
                    </a>
                  </li>
                ))}
              </ul>
            </div>
          )}
          
          {results.totalCount === 0 && (
            <div className="no-results">
              <p>No results found</p>
            </div>
          )}
        </div>
      )}
    </div>
  );
}
```

### Step 4: Update Your Existing Implementation (If Already Using Global Search)

If you already have global search implemented, you only need to:

1. **Add authentication header** to your fetch/axios calls
2. **Add error handling** for 401/403 responses

**Example Migration:**

```typescript
// ❌ Before
const response = await fetch(`/api/search?q=${query}`);

// ✅ After
const token = localStorage.getItem('token');
const response = await fetch(`/api/search?q=${query}`, {
  headers: {
    'Authorization': `Bearer ${token}`
  }
});

if (response.status === 401) {
  // Handle authentication error
  window.location.href = '/login';
  return;
}
```

## 🔍 API Reference

### Endpoint
```
GET /api/search
```

### Authentication
**Required**: Bearer Token
```
Authorization: Bearer <your-jwt-token>
```

### Query Parameters
| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `q` | string | Yes | Search query string |
| `limit` | number | No | Max results per entity type (default: 10) |

### Request Example
```javascript
GET /api/search?q=john&limit=10
Headers:
  Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
```

### Response Format (Unchanged)
```json
{
  "success": true,
  "message": "Found 5 persons and 3 deals",
  "data": {
    "persons": [
      {
        "id": 1,
        "name": "John Doe",
        "instagramId": "@johndoe",
        "phone": "+1234567890",
        "email": "john@example.com",
        "organizationId": 10,
        "organizationName": "Vendor ABC",
        "ownerId": 5,
        "ownerDisplayName": "Sales Manager",
        ...
      }
    ],
    "deals": [
      {
        "id": 1,
        "name": "Wedding Package",
        "value": 5000,
        "personId": 1,
        "personName": "John Doe",
        "phoneNumber": "+1234567890",
        "organizationId": 10,
        "organizationName": "Vendor ABC",
        "status": "IN_PROGRESS",
        ...
      }
    ],
    "personsCount": 5,
    "dealsCount": 3,
    "totalCount": 8
  }
}
```

### Error Responses

#### 401 Unauthorized
```json
{
  "success": false,
  "message": "User not authenticated"
}
```

#### 403 Forbidden
```json
{
  "success": false,
  "message": "Access denied. You don't have permission to access this resource."
}
```

#### 400 Bad Request
```json
{
  "success": false,
  "message": "Search query 'q' parameter is required and cannot be empty"
}
```

## ✅ Testing Checklist

### 1. Authentication Tests
- [ ] **Unauthenticated Request**: Returns 401 Unauthorized
- [ ] **Expired Token**: Returns 401 and triggers login redirect
- [ ] **Invalid Token**: Returns 401 and triggers login redirect
- [ ] **Valid Token**: Returns 200 OK with filtered results

### 2. Search Functionality Tests
- [ ] **Empty Query**: Returns empty results (no error)
- [ ] **Valid Query**: Returns filtered results
- [ ] **No Results**: Returns empty arrays with counts = 0
- [ ] **Limit Parameter**: Respects the limit parameter

### 3. Role-Based Filtering Tests
- [ ] **ADMIN User**: Sees all persons and deals
- [ ] **CATEGORY_MANAGER User**: Sees only accessible organizations
- [ ] **SALES User**: Sees only accessible organizations
- [ ] **PRESALES User**: Sees only their own organizations

### 4. Error Handling Tests
- [ ] **Network Error**: Shows appropriate error message
- [ ] **401 Error**: Redirects to login or shows login modal
- [ ] **403 Error**: Shows access denied message
- [ ] **500 Error**: Shows generic error message

## 🐛 Common Issues and Solutions

### Issue 1: "401 Unauthorized" Error

**Symptoms:**
- All search requests return 401
- Error message: "User not authenticated"

**Causes:**
- Missing authentication token
- Token not included in request headers
- Token expired

**Solutions:**
```typescript
// ✅ Check if token exists before making request
const token = localStorage.getItem('token');
if (!token) {
  // Redirect to login
  window.location.href = '/login';
  return;
}

// ✅ Include token in headers
const response = await fetch(url, {
  headers: {
    'Authorization': `Bearer ${token}`
  }
});
```

### Issue 2: Users See Fewer Results Than Expected

**Symptoms:**
- Search returns fewer results than before
- Some expected results are missing

**Cause:**
- RBAC filtering is working correctly
- User only has access to certain organizations

**Solution:**
This is **expected behavior**. Verify:
- User's role is correct
- Organization ownership is correctly assigned
- User hierarchy (manager relationships) is set up correctly

**To verify access:**
- Check user's role in the system
- Verify which organizations the user owns
- Check the user's manager hierarchy

### Issue 3: Search Returns Empty Results

**Symptoms:**
- Search always returns empty results
- `personsCount` and `dealsCount` are always 0

**Possible Causes:**
1. No matching records exist for the user's accessible organizations
2. User has no accessible organizations assigned
3. Search query doesn't match any records

**Solutions:**
```typescript
// Check if user has access
// Call /api/organizations/accessible-for-current-user to see accessible orgs
const orgsResponse = await fetch('/api/organizations/accessible-for-current-user', {
  headers: {
    'Authorization': `Bearer ${token}`
  }
});

const orgs = await orgsResponse.json();
console.log('Accessible organizations:', orgs.data);

// If empty, user has no organizations assigned
if (orgs.data.length === 0) {
  console.warn('User has no accessible organizations');
}
```

### Issue 4: Token Expires During Search

**Symptoms:**
- Search works initially, then starts returning 401
- Intermittent authentication errors

**Solution:**
```typescript
// Add token refresh logic or check token expiration
function isTokenExpired(token: string): boolean {
  try {
    const payload = JSON.parse(atob(token.split('.')[1]));
    const expirationTime = payload.exp * 1000;
    return Date.now() >= expirationTime;
  } catch {
    return true;
  }
}

// Before making request
const token = localStorage.getItem('token');
if (!token || isTokenExpired(token)) {
  // Refresh token or redirect to login
  window.location.href = '/login';
  return;
}
```

## 📊 Understanding Filtered Results

### Why Results Are Filtered

The backend automatically filters results based on:
1. **User's role** (ADMIN, CATEGORY_MANAGER, SALES, PRESALES)
2. **Organization ownership** (who owns the organization)
3. **User hierarchy** (manager relationships)

### Example Scenarios

#### Scenario 1: Category Manager
- Category Manager A has Sales B under them
- Sales B has Presales C under them
- **Category Manager A sees:**
  - Organizations owned by A
  - Organizations owned by B
  - Organizations owned by C

#### Scenario 2: Sales User
- Sales A has Presales B under them
- **Sales A sees:**
  - Organizations owned by A
  - Organizations owned by B

#### Scenario 3: Presales User
- Presales A has Sales Manager B as their manager
- Sales Manager B has Presales A and Presales C under them
- **Presales A sees (same as Sales Manager B):**
  - Organizations owned by Sales Manager B
  - Organizations owned by Presales A (themselves)
  - Organizations owned by Presales C
- **If Presales A has no Sales manager:**
  - Organizations owned by Presales A only

## 🔄 Migration Checklist

### For Existing Implementations

- [ ] Update all `/api/search` API calls to include `Authorization` header
- [ ] Add error handling for 401/403 responses
- [ ] Test with different user roles (ADMIN, CATEGORY_MANAGER, SALES, PRESALES)
- [ ] Verify search results are filtered correctly for each role
- [ ] Update error messages to handle authentication failures
- [ ] Test token expiration scenarios
- [ ] Verify empty results handling
- [ ] Update user documentation if needed

### Quick Migration Template

```typescript
// Replace this pattern:
const response = await fetch(`/api/search?q=${query}`);

// With this:
const token = localStorage.getItem('token');
if (!token) {
  // Handle no token
  return;
}

const response = await fetch(`/api/search?q=${query}`, {
  headers: {
    'Authorization': `Bearer ${token}`
  }
});

if (response.status === 401) {
  // Handle authentication error
  return;
}
```

## 📚 Additional Resources

### Related APIs
- **Get Accessible Organizations**: `GET /api/organizations/accessible-for-current-user`
  - Use this to see which organizations the current user can access
  - Helps debug why certain results might be missing

### TypeScript Types (Unchanged)

```typescript
// types/search.ts

export interface PersonDTO {
  id: number;
  name: string;
  instagramId?: string;
  phone?: string;
  email?: string;
  organizationId?: number;
  organizationName?: string;
  ownerId?: number;
  ownerDisplayName?: string;
  // ... other fields
}

export interface DealResponse {
  id: number;
  name: string;
  value: number;
  personId?: number;
  personName?: string;
  phoneNumber?: string;
  organizationId?: number;
  organizationName?: string;
  status: string;
  // ... other fields
}

export interface GlobalSearchResponse {
  persons: PersonDTO[];
  deals: DealResponse[];
  personsCount: number;
  dealsCount: number;
  totalCount: number;
}

export interface ApiResponse<T> {
  success: boolean;
  message: string;
  data: T;
}
```

## 🎯 Summary

### What You Need to Do
1. ✅ **Add authentication** to all global search API calls
2. ✅ **Handle 401/403 errors** appropriately
3. ✅ **Test with different roles** to verify filtering

### What Stays the Same
- ✅ API endpoint URL (`/api/search`)
- ✅ Query parameters (`q`, `limit`)
- ✅ Response structure and format
- ✅ Search functionality
- ✅ TypeScript types/interfaces

### What Changed
- ⚠️ **Authentication is now required** (breaking change)
- ✅ Results are filtered by role (automatic, no frontend changes needed)

### Expected Behavior
- Users will see **fewer results** than before if they don't have access to all organizations
- This is **by design** and ensures data security
- ADMIN users will see all results (no change for them)

## 🆘 Support

If you encounter issues:

1. **Check Authentication**: Verify token is valid and included in requests
2. **Check User Role**: Verify user's role is correctly assigned
3. **Check Organization Ownership**: Verify organizations are assigned to correct users
4. **Test with Different Roles**: Verify behavior matches expected role-based filtering
5. **Check Network Tab**: Inspect request/response in browser DevTools

For backend issues, check backend logs for RBAC filtering debug messages.

---

**Last Updated**: [Current Date]  
**API Version**: 1.0  
**Breaking Changes**: Authentication now required

