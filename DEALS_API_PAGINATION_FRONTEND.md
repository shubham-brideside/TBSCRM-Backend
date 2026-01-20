# Deals API Pagination - Frontend Implementation Guide

## Overview
The Deals API now supports pagination with `limit` and `offset` parameters. This allows the frontend to load deals in batches (e.g., 100 at a time) instead of loading all deals at once, improving performance and user experience.

## API Changes

### Endpoint
```
GET /api/deals
```

### New Query Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `limit` | Integer | No | 100 | Number of deals to return per page |
| `offset` | Integer | No | 0 | Number of deals to skip (for pagination) |

### Existing Query Parameters (Unchanged)
- `pipelineId` - Filter by pipeline ID
- `status` - Filter by status (IN_PROGRESS/WON/LOST/all)
- `organizationId` - Filter by organization ID
- `categoryId` - Filter by category ID
- `managerId` - Filter by manager ID
- `dateFrom` - Start date (YYYY-MM-DD)
- `dateTo` - End date (YYYY-MM-DD)
- `search` - Search query
- `source` - Filter by source
- `sort` - Sort field and direction (e.g., "nextActivity,asc")

### Response Structure

**Before (Old Response):**
```json
[
  {
    "id": 1,
    "name": "Deal Name",
    // ... other deal fields
  }
]
```

**After (New Response):**
```json
{
  "deals": [
    {
      "id": 1,
      "name": "Deal Name",
      // ... other deal fields
    },
    // ... more deals (up to limit)
  ],
  "totalCount": 350
}
```

## Frontend Implementation

### 1. Update API Call Function

**Before:**
```typescript
const loadDeals = async () => {
  const response = await fetch(
    `/api/deals?sort=${sort}&pipelineId=${pipelineId}&status=${status}&dateFrom=${dateFrom}&dateTo=${dateTo}`
  );
  const deals = await response.json();
  setDeals(deals);
};
```

**After:**
```typescript
interface DealsListResponse {
  deals: Deal[];
  totalCount: number;
}

const loadDeals = async (page: number = 0, limit: number = 100) => {
  const offset = page * limit;
  const response = await fetch(
    `/api/deals?sort=${sort}&pipelineId=${pipelineId}&status=${status}&dateFrom=${dateFrom}&dateTo=${dateTo}&limit=${limit}&offset=${offset}`
  );
  const data: DealsListResponse = await response.json();
  return data;
};
```

### 2. Implementation Options

#### Option A: Infinite Scroll (Recommended)

Load more deals automatically when user scrolls to the bottom.

```typescript
import { useState, useEffect, useCallback } from 'react';

interface Deal {
  id: number;
  name: string;
  // ... other deal fields
}

const DealsPage = () => {
  const [deals, setDeals] = useState<Deal[]>([]);
  const [totalCount, setTotalCount] = useState(0);
  const [loading, setLoading] = useState(false);
  const [hasMore, setHasMore] = useState(true);
  const [currentPage, setCurrentPage] = useState(0);
  const limit = 100;

  const loadDeals = useCallback(async (page: number, append: boolean = false) => {
    if (loading) return;
    
    setLoading(true);
    try {
      const offset = page * limit;
      const queryParams = new URLSearchParams({
        sort: 'nextActivity,asc',
        limit: limit.toString(),
        offset: offset.toString(),
        // ... other filters
      });

      const response = await fetch(`/api/deals?${queryParams}`);
      const data = await response.json();

      if (append) {
        setDeals(prev => [...prev, ...data.deals]);
      } else {
        setDeals(data.deals);
      }

      setTotalCount(data.totalCount);
      setHasMore(data.deals.length === limit && deals.length + data.deals.length < data.totalCount);
      setCurrentPage(page);
    } catch (error) {
      console.error('Error loading deals:', error);
    } finally {
      setLoading(false);
    }
  }, [loading, deals.length]);

  // Initial load
  useEffect(() => {
    loadDeals(0, false);
  }, []); // Re-run when filters change

  // Load more on scroll
  const handleScroll = useCallback(() => {
    if (
      window.innerHeight + document.documentElement.scrollTop >=
      document.documentElement.offsetHeight - 1000 && // Load when 1000px from bottom
      hasMore &&
      !loading
    ) {
      loadDeals(currentPage + 1, true);
    }
  }, [hasMore, loading, currentPage, loadDeals]);

  useEffect(() => {
    window.addEventListener('scroll', handleScroll);
    return () => window.removeEventListener('scroll', handleScroll);
  }, [handleScroll]);

  return (
    <div>
      <div className="deals-header">
        <h2>Deals</h2>
        <p>Showing {deals.length} of {totalCount} deals</p>
      </div>
      
      <div className="deals-list">
        {deals.map(deal => (
          <DealCard key={deal.id} deal={deal} />
        ))}
      </div>

      {loading && <div className="loading">Loading more deals...</div>}
      {!hasMore && deals.length > 0 && (
        <div className="end-message">No more deals to load</div>
      )}
    </div>
  );
};
```

#### Option B: Pagination with Page Numbers

Traditional pagination with page numbers and "Previous/Next" buttons.

```typescript
const DealsPage = () => {
  const [deals, setDeals] = useState<Deal[]>([]);
  const [totalCount, setTotalCount] = useState(0);
  const [currentPage, setCurrentPage] = useState(0);
  const [loading, setLoading] = useState(false);
  const limit = 100;

  const totalPages = Math.ceil(totalCount / limit);

  const loadDeals = async (page: number) => {
    setLoading(true);
    try {
      const offset = page * limit;
      const queryParams = new URLSearchParams({
        sort: 'nextActivity,asc',
        limit: limit.toString(),
        offset: offset.toString(),
        // ... other filters
      });

      const response = await fetch(`/api/deals?${queryParams}`);
      const data = await response.json();

      setDeals(data.deals);
      setTotalCount(data.totalCount);
      setCurrentPage(page);
    } catch (error) {
      console.error('Error loading deals:', error);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    loadDeals(0);
  }, []); // Re-run when filters change

  const handlePageChange = (newPage: number) => {
    if (newPage >= 0 && newPage < totalPages) {
      loadDeals(newPage);
      window.scrollTo({ top: 0, behavior: 'smooth' });
    }
  };

  return (
    <div>
      <div className="deals-header">
        <h2>Deals</h2>
        <p>
          Showing {deals.length > 0 ? currentPage * limit + 1 : 0} - {Math.min((currentPage + 1) * limit, totalCount)} of {totalCount} deals
        </p>
      </div>

      {loading ? (
        <div className="loading">Loading deals...</div>
      ) : (
        <>
          <div className="deals-list">
            {deals.map(deal => (
              <DealCard key={deal.id} deal={deal} />
            ))}
          </div>

          <div className="pagination">
            <button
              onClick={() => handlePageChange(currentPage - 1)}
              disabled={currentPage === 0}
            >
              Previous
            </button>
            
            <span>
              Page {currentPage + 1} of {totalPages}
            </span>

            <button
              onClick={() => handlePageChange(currentPage + 1)}
              disabled={currentPage >= totalPages - 1}
            >
              Next
            </button>
          </div>
        </>
      )}
    </div>
  );
};
```

#### Option C: Load More Button

Load more deals when user clicks a "Load More" button.

```typescript
const DealsPage = () => {
  const [deals, setDeals] = useState<Deal[]>([]);
  const [totalCount, setTotalCount] = useState(0);
  const [loading, setLoading] = useState(false);
  const [currentPage, setCurrentPage] = useState(0);
  const limit = 100;

  const hasMore = deals.length < totalCount;

  const loadDeals = async (page: number, append: boolean = false) => {
    setLoading(true);
    try {
      const offset = page * limit;
      const queryParams = new URLSearchParams({
        sort: 'nextActivity,asc',
        limit: limit.toString(),
        offset: offset.toString(),
        // ... other filters
      });

      const response = await fetch(`/api/deals?${queryParams}`);
      const data = await response.json();

      if (append) {
        setDeals(prev => [...prev, ...data.deals]);
      } else {
        setDeals(data.deals);
      }

      setTotalCount(data.totalCount);
      setCurrentPage(page);
    } catch (error) {
      console.error('Error loading deals:', error);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    loadDeals(0, false);
  }, []); // Re-run when filters change

  const handleLoadMore = () => {
    loadDeals(currentPage + 1, true);
  };

  return (
    <div>
      <div className="deals-header">
        <h2>Deals</h2>
        <p>Showing {deals.length} of {totalCount} deals</p>
      </div>

      <div className="deals-list">
        {deals.map(deal => (
          <DealCard key={deal.id} deal={deal} />
        ))}
      </div>

      {hasMore && (
        <div className="load-more-container">
          <button
            onClick={handleLoadMore}
            disabled={loading}
            className="load-more-button"
          >
            {loading ? 'Loading...' : 'Load More'}
          </button>
        </div>
      )}
    </div>
  );
};
```

### 3. Handle Filter Changes

When filters change, reset to page 0:

```typescript
const [filters, setFilters] = useState({
  pipelineId: null,
  status: 'IN_PROGRESS',
  dateFrom: null,
  dateTo: null,
  // ... other filters
});

useEffect(() => {
  // Reset to first page when filters change
  loadDeals(0, false);
}, [filters.pipelineId, filters.status, filters.dateFrom, filters.dateTo]);
```

### 4. Update TypeScript Types

```typescript
// types/deal.ts
export interface Deal {
  id: number;
  name: string;
  value: number;
  personId: number | null;
  personName: string | null;
  pipelineId: number | null;
  stageId: number | null;
  organizationId: number | null;
  organizationName: string | null;
  status: 'IN_PROGRESS' | 'WON' | 'LOST';
  createdAt: string;
  updatedAt: string;
  // ... other fields
}

export interface DealsListResponse {
  deals: Deal[];
  totalCount: number;
}
```

## Migration Steps

### Step 1: Update API Response Handling

1. Update the function that calls `/api/deals`
2. Change from expecting an array to expecting an object with `deals` and `totalCount`
3. Update TypeScript types/interfaces

### Step 2: Add Pagination State

1. Add state for `currentPage`, `totalCount`, `loading`, `hasMore`
2. Initialize pagination state

### Step 3: Implement Pagination Logic

Choose one of the three options:
- **Infinite Scroll** (Recommended for mobile-friendly UX)
- **Page Numbers** (Traditional, good for desktop)
- **Load More Button** (Simple, explicit user action)

### Step 4: Update UI

1. Display total count: "Showing X of Y deals"
2. Add loading indicators
3. Add pagination controls (if using page numbers)
4. Handle empty states

### Step 5: Test

1. Test with different filter combinations
2. Test pagination with large datasets (300+ deals)
3. Test edge cases:
   - Last page (fewer deals than limit)
   - No results
   - Filter changes reset pagination

## Example API Calls

### First Page (Default)
```
GET /api/deals?pipelineId=44&status=IN_PROGRESS&limit=100&offset=0&sort=nextActivity,asc
```

### Second Page
```
GET /api/deals?pipelineId=44&status=IN_PROGRESS&limit=100&offset=100&sort=nextActivity,asc
```

### Third Page
```
GET /api/deals?pipelineId=44&status=IN_PROGRESS&limit=100&offset=200&sort=nextActivity,asc
```

## Best Practices

1. **Default Limit**: Use 100 deals per page for optimal performance
2. **Reset on Filter Change**: Always reset to page 0 when filters change
3. **Loading States**: Show loading indicators during API calls
4. **Error Handling**: Handle API errors gracefully
5. **Empty States**: Show appropriate messages when no deals are found
6. **Performance**: 
   - Use `useCallback` for scroll handlers
   - Debounce scroll events if needed
   - Consider virtual scrolling for very large lists

## Breaking Changes

⚠️ **Important**: The API response structure has changed from an array to an object.

**Before:**
```typescript
const deals: Deal[] = await response.json();
```

**After:**
```typescript
const { deals, totalCount }: DealsListResponse = await response.json();
```

Make sure to update all places where the deals API response is used.

## Testing Checklist

- [ ] First page loads correctly (100 deals)
- [ ] Second page loads correctly (next 100 deals)
- [ ] Total count displays correctly
- [ ] Pagination resets when filters change
- [ ] Loading states work correctly
- [ ] Empty states display correctly
- [ ] Last page handles correctly (fewer than limit deals)
- [ ] Scroll/load more works smoothly
- [ ] No duplicate deals across pages
- [ ] Performance is acceptable with large datasets

## Support

If you encounter any issues:
1. Check browser console for errors
2. Verify API response structure matches expected format
3. Ensure `limit` and `offset` parameters are being sent correctly
4. Check that `totalCount` is being used for pagination calculations

