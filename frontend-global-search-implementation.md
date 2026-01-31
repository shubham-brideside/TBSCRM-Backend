# Frontend Implementation Guide: Global Search API

## Overview

This guide provides step-by-step instructions for implementing the Global Search API in your frontend application. The API allows you to search across both persons and deals in a single call, searching by name, instagram_id, and phone_number.

## API Endpoint

```
GET /api/search?q={query}&limit={limit}
```

**Parameters:**
- `q` (required): Search query string
- `limit` (optional): Maximum results per entity type (default: 10)

**Response:**
```json
{
  "success": true,
  "message": "Found 5 persons and 3 deals",
  "data": {
    "persons": [...],
    "deals": [...],
    "personsCount": 5,
    "dealsCount": 3,
    "totalCount": 8
  }
}
```

## Implementation Steps

### Step 1: Create TypeScript Interfaces

First, define the types for the search results:

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
  // ... other person fields
}

export interface DealResponse {
  id: number;
  name: string;
  value: number;
  personId?: number;
  personName?: string;
  phoneNumber?: string;
  venue?: string;
  organizationId?: number;
  organizationName?: string;
  pipelineId?: number;
  stageId?: number;
  status: string;
  // ... other deal fields
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

### Step 2: Create API Service Function

Create a service function to call the global search API:

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

  const url = new URL(`${API_BASE_URL}/api/search`);
  url.searchParams.append('q', query.trim());
  if (limit > 0) {
    url.searchParams.append('limit', limit.toString());
  }

  const response = await fetch(url.toString(), {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${getAuthToken()}`, // Your auth token getter
    },
  });

  if (!response.ok) {
    const error = await response.json();
    throw new Error(error.message || 'Search failed');
  }

  const data: ApiResponse<GlobalSearchResponse> = await response.json();
  
  if (!data.success) {
    throw new Error(data.message || 'Search failed');
  }

  return data.data;
}

// Helper function to get auth token (implement based on your auth system)
function getAuthToken(): string {
  return localStorage.getItem('authToken') || '';
}
```

### Step 3: Create React Hook (Recommended)

Create a custom hook for global search with debouncing and loading states:

```typescript
// hooks/useGlobalSearch.ts

import { useState, useEffect, useCallback } from 'react';
import { globalSearch } from '../services/searchService';
import { GlobalSearchResponse } from '../types/search';

interface UseGlobalSearchOptions {
  debounceMs?: number;
  defaultLimit?: number;
  minQueryLength?: number;
}

export function useGlobalSearch(options: UseGlobalSearchOptions = {}) {
  const {
    debounceMs = 300,
    defaultLimit = 10,
    minQueryLength = 2,
  } = options;

  const [query, setQuery] = useState('');
  const [results, setResults] = useState<GlobalSearchResponse | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const performSearch = useCallback(
    async (searchQuery: string, limit: number = defaultLimit) => {
      if (searchQuery.trim().length < minQueryLength) {
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
        setError(err instanceof Error ? err.message : 'Search failed');
        setResults(null);
      } finally {
        setLoading(false);
      }
    },
    [defaultLimit, minQueryLength]
  );

  // Debounced search effect
  useEffect(() => {
    if (query.trim().length < minQueryLength) {
      setResults(null);
      return;
    }

    const timeoutId = setTimeout(() => {
      performSearch(query);
    }, debounceMs);

    return () => clearTimeout(timeoutId);
  }, [query, debounceMs, minQueryLength, performSearch]);

  const search = useCallback(
    (searchQuery: string, limit?: number) => {
      setQuery(searchQuery);
      if (searchQuery.trim().length >= minQueryLength) {
        performSearch(searchQuery, limit);
      }
    },
    [minQueryLength, performSearch]
  );

  const clearSearch = useCallback(() => {
    setQuery('');
    setResults(null);
    setError(null);
  }, []);

  return {
    query,
    results,
    loading,
    error,
    search,
    clearSearch,
  };
}
```

### Step 4: Create Search Component

Create a reusable search component:

```typescript
// components/GlobalSearch.tsx

import React, { useState } from 'react';
import { useGlobalSearch } from '../hooks/useGlobalSearch';
import { PersonDTO, DealResponse } from '../types/search';

interface GlobalSearchProps {
  onPersonSelect?: (person: PersonDTO) => void;
  onDealSelect?: (deal: DealResponse) => void;
  placeholder?: string;
  showResults?: boolean;
}

export function GlobalSearch({
  onPersonSelect,
  onDealSelect,
  placeholder = 'Search persons and deals...',
  showResults = true,
}: GlobalSearchProps) {
  const { query, results, loading, error, search, clearSearch } = useGlobalSearch({
    debounceMs: 300,
    minQueryLength: 2,
  });

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    search(e.target.value);
  };

  return (
    <div className="global-search">
      <div className="search-input-container">
        <input
          type="text"
          value={query}
          onChange={handleInputChange}
          placeholder={placeholder}
          className="search-input"
        />
        {loading && <span className="loading-spinner">⏳</span>}
        {query && (
          <button onClick={clearSearch} className="clear-button">
            ✕
          </button>
        )}
      </div>

      {error && (
        <div className="error-message">
          {error}
        </div>
      )}

      {showResults && results && (
        <SearchResults
          results={results}
          onPersonSelect={onPersonSelect}
          onDealSelect={onDealSelect}
        />
      )}
    </div>
  );
}

interface SearchResultsProps {
  results: {
    persons: PersonDTO[];
    deals: DealResponse[];
    personsCount: number;
    dealsCount: number;
    totalCount: number;
  };
  onPersonSelect?: (person: PersonDTO) => void;
  onDealSelect?: (deal: DealResponse) => void;
}

function SearchResults({ results, onPersonSelect, onDealSelect }: SearchResultsProps) {
  if (results.totalCount === 0) {
    return (
      <div className="no-results">
        No results found
      </div>
    );
  }

  return (
    <div className="search-results">
      {results.personsCount > 0 && (
        <div className="results-section">
          <h3 className="section-title">
            Persons ({results.personsCount})
          </h3>
          <ul className="results-list">
            {results.persons.map((person) => (
              <li
                key={person.id}
                className="result-item person-item"
                onClick={() => onPersonSelect?.(person)}
              >
                <div className="item-name">{person.name}</div>
                {person.instagramId && (
                  <div className="item-meta">📷 {person.instagramId}</div>
                )}
                {person.phone && (
                  <div className="item-meta">📞 {person.phone}</div>
                )}
                {person.organizationName && (
                  <div className="item-meta">🏢 {person.organizationName}</div>
                )}
              </li>
            ))}
          </ul>
        </div>
      )}

      {results.dealsCount > 0 && (
        <div className="results-section">
          <h3 className="section-title">
            Deals ({results.dealsCount})
          </h3>
          <ul className="results-list">
            {results.deals.map((deal) => (
              <li
                key={deal.id}
                className="result-item deal-item"
                onClick={() => onDealSelect?.(deal)}
              >
                <div className="item-name">{deal.name}</div>
                {deal.personName && (
                  <div className="item-meta">👤 {deal.personName}</div>
                )}
                {deal.phoneNumber && (
                  <div className="item-meta">📞 {deal.phoneNumber}</div>
                )}
                {deal.venue && (
                  <div className="item-meta">📍 {deal.venue}</div>
                )}
                {deal.value && (
                  <div className="item-meta">💰 ₹{deal.value.toLocaleString()}</div>
                )}
              </li>
            ))}
          </ul>
        </div>
      )}
    </div>
  );
}
```

### Step 5: Add CSS Styling

```css
/* styles/GlobalSearch.css */

.global-search {
  position: relative;
  width: 100%;
  max-width: 600px;
}

.search-input-container {
  position: relative;
  display: flex;
  align-items: center;
}

.search-input {
  width: 100%;
  padding: 12px 40px 12px 16px;
  border: 2px solid #e0e0e0;
  border-radius: 8px;
  font-size: 16px;
  transition: border-color 0.2s;
}

.search-input:focus {
  outline: none;
  border-color: #4a90e2;
}

.loading-spinner {
  position: absolute;
  right: 40px;
  color: #999;
}

.clear-button {
  position: absolute;
  right: 12px;
  background: none;
  border: none;
  cursor: pointer;
  font-size: 18px;
  color: #999;
  padding: 4px;
}

.clear-button:hover {
  color: #333;
}

.error-message {
  margin-top: 8px;
  padding: 8px;
  background-color: #fee;
  color: #c33;
  border-radius: 4px;
  font-size: 14px;
}

.search-results {
  margin-top: 16px;
  border: 1px solid #e0e0e0;
  border-radius: 8px;
  background: white;
  box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  max-height: 500px;
  overflow-y: auto;
}

.results-section {
  padding: 16px;
}

.results-section:not(:last-child) {
  border-bottom: 1px solid #e0e0e0;
}

.section-title {
  margin: 0 0 12px 0;
  font-size: 14px;
  font-weight: 600;
  color: #666;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.results-list {
  list-style: none;
  padding: 0;
  margin: 0;
}

.result-item {
  padding: 12px;
  border-radius: 6px;
  cursor: pointer;
  transition: background-color 0.2s;
  margin-bottom: 8px;
}

.result-item:hover {
  background-color: #f5f5f5;
}

.result-item:last-child {
  margin-bottom: 0;
}

.item-name {
  font-weight: 600;
  color: #333;
  margin-bottom: 4px;
}

.item-meta {
  font-size: 13px;
  color: #666;
  margin-top: 4px;
}

.no-results {
  padding: 24px;
  text-align: center;
  color: #999;
}
```

### Step 6: Usage Examples

#### Example 1: Simple Search Bar

```typescript
// App.tsx or your main component

import { GlobalSearch } from './components/GlobalSearch';

function App() {
  const handlePersonSelect = (person) => {
    console.log('Selected person:', person);
    // Navigate to person detail page
    // navigate(`/persons/${person.id}`);
  };

  const handleDealSelect = (deal) => {
    console.log('Selected deal:', deal);
    // Navigate to deal detail page
    // navigate(`/deals/${deal.id}`);
  };

  return (
    <div className="app">
      <header>
        <GlobalSearch
          onPersonSelect={handlePersonSelect}
          onDealSelect={handleDealSelect}
          placeholder="Search persons and deals..."
        />
      </header>
      {/* Rest of your app */}
    </div>
  );
}
```

#### Example 2: Using the Hook Directly

```typescript
// components/CustomSearch.tsx

import { useGlobalSearch } from '../hooks/useGlobalSearch';

function CustomSearch() {
  const { query, results, loading, search } = useGlobalSearch();

  return (
    <div>
      <input
        type="text"
        value={query}
        onChange={(e) => search(e.target.value)}
        placeholder="Search..."
      />
      
      {loading && <div>Loading...</div>}
      
      {results && (
        <div>
          <h3>Persons: {results.personsCount}</h3>
          {results.persons.map((person) => (
            <div key={person.id}>{person.name}</div>
          ))}
          
          <h3>Deals: {results.dealsCount}</h3>
          {results.deals.map((deal) => (
            <div key={deal.id}>{deal.name}</div>
          ))}
        </div>
      )}
    </div>
  );
}
```

#### Example 3: Search with Manual Trigger

```typescript
// components/ManualSearch.tsx

import { useState } from 'react';
import { globalSearch } from '../services/searchService';

function ManualSearch() {
  const [query, setQuery] = useState('');
  const [results, setResults] = useState(null);
  const [loading, setLoading] = useState(false);

  const handleSearch = async () => {
    if (!query.trim()) return;

    setLoading(true);
    try {
      const searchResults = await globalSearch(query, 20);
      setResults(searchResults);
    } catch (error) {
      console.error('Search failed:', error);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div>
      <input
        type="text"
        value={query}
        onChange={(e) => setQuery(e.target.value)}
        onKeyPress={(e) => e.key === 'Enter' && handleSearch()}
      />
      <button onClick={handleSearch} disabled={loading}>
        {loading ? 'Searching...' : 'Search'}
      </button>
      
      {results && (
        <div>
          <p>Found {results.totalCount} results</p>
          {/* Display results */}
        </div>
      )}
    </div>
  );
}
```

## Advanced Features

### 1. Highlighting Search Terms

```typescript
// utils/highlight.ts

export function highlightText(text: string, query: string): string {
  if (!query || !text) return text;
  
  const regex = new RegExp(`(${query})`, 'gi');
  return text.replace(regex, '<mark>$1</mark>');
}

// Usage in component
<div 
  dangerouslySetInnerHTML={{
    __html: highlightText(person.name, query)
  }}
/>
```

### 2. Keyboard Navigation

```typescript
// Enhanced hook with keyboard navigation

export function useGlobalSearchWithKeyboard(options = {}) {
  const searchHook = useGlobalSearch(options);
  const [selectedIndex, setSelectedIndex] = useState(-1);

  const handleKeyDown = useCallback((e: KeyboardEvent) => {
    if (!searchHook.results) return;

    const totalItems = searchHook.results.totalCount;
    
    if (e.key === 'ArrowDown') {
      e.preventDefault();
      setSelectedIndex((prev) => 
        prev < totalItems - 1 ? prev + 1 : prev
      );
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      setSelectedIndex((prev) => (prev > 0 ? prev - 1 : -1));
    } else if (e.key === 'Enter' && selectedIndex >= 0) {
      // Handle selection
      const item = getItemAtIndex(searchHook.results, selectedIndex);
      // ... handle selection
    }
  }, [searchHook.results, selectedIndex]);

  return {
    ...searchHook,
    selectedIndex,
    handleKeyDown,
  };
}
```

### 3. Caching Search Results

```typescript
// Enhanced service with caching

const searchCache = new Map<string, { data: GlobalSearchResponse; timestamp: number }>();
const CACHE_DURATION = 5 * 60 * 1000; // 5 minutes

export async function globalSearchWithCache(
  query: string,
  limit: number = 10
): Promise<GlobalSearchResponse> {
  const cacheKey = `${query}:${limit}`;
  const cached = searchCache.get(cacheKey);

  if (cached && Date.now() - cached.timestamp < CACHE_DURATION) {
    return cached.data;
  }

  const results = await globalSearch(query, limit);
  searchCache.set(cacheKey, {
    data: results,
    timestamp: Date.now(),
  });

  return results;
}
```

## Best Practices

1. **Debouncing**: Always debounce search input to avoid excessive API calls
2. **Minimum Query Length**: Require at least 2-3 characters before searching
3. **Loading States**: Show loading indicators during search
4. **Error Handling**: Display user-friendly error messages
5. **Empty States**: Show helpful messages when no results are found
6. **Accessibility**: Add proper ARIA labels and keyboard navigation
7. **Performance**: Limit results per entity type (default: 10)
8. **Caching**: Consider caching recent searches for better UX

## Testing

```typescript
// __tests__/globalSearch.test.ts

import { globalSearch } from '../services/searchService';

describe('GlobalSearch', () => {
  it('should search for persons and deals', async () => {
    const results = await globalSearch('john', 10);
    
    expect(results).toHaveProperty('persons');
    expect(results).toHaveProperty('deals');
    expect(results).toHaveProperty('totalCount');
  });

  it('should handle empty query', async () => {
    await expect(globalSearch('')).rejects.toThrow();
  });
});
```

## Troubleshooting

### Issue: No results returned
- Check if query meets minimum length requirement
- Verify API endpoint is correct
- Check network tab for API errors

### Issue: Slow search performance
- Increase debounce time
- Reduce limit parameter
- Check backend performance

### Issue: Authentication errors
- Verify auth token is being sent
- Check token expiration
- Ensure proper headers are set

## Summary

The global search API provides a unified way to search across persons and deals. By following this guide, you can:

1. ✅ Set up TypeScript types
2. ✅ Create API service functions
3. ✅ Build reusable React hooks
4. ✅ Create search components
5. ✅ Add styling and UX enhancements
6. ✅ Implement advanced features

The implementation is flexible and can be customized based on your specific needs.

