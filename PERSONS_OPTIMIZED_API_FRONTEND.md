# Persons Optimized API - Frontend Implementation Guide

## Overview

This document describes the new optimized API endpoint for loading persons with their related deals and activities in a single request. This replaces the previous approach where the frontend was calling `/api/deals` (returning all deals) and `/api/activities` separately, causing significant performance issues on the person and person details pages.

## Problem Statement

**Current Issues:**
1. Person pages call `/api/deals` which returns ALL deals, even those not related to the loaded persons
2. Separate `/api/activities` call may return activities not related to the loaded persons
3. Multiple API calls cause slow page load times
4. Unnecessary data transfer and processing

**Solution:**
- Single optimized API endpoint that returns only persons matching filters
- Includes related deals and activities using JOINs (efficient database queries)
- Supports pagination for infinite scroll (200 persons per page)
- All data needed for the person page in one response

---

## New API Endpoint

### Endpoint: `GET /api/persons/with-details`

**Description:** Returns paginated persons with their associated deals and activities in a single optimized response. Uses JOINs to efficiently fetch related data.

### Request Parameters

All parameters are optional and support the same filtering as `/api/persons`:

| Parameter | Type | Description | Example |
|-----------|------|-------------|---------|
| `q` | string | Search term (name, phone, email, Instagram, organization name, owner name) | `q=john` |
| `label` | string | Comma-separated label codes (e.g., `BRIDAL_MAKEUP,BRIDAL_PLANNING`) | `label=BRIDAL_MAKEUP` |
| `source` | string | Source enum code | `source=INSTAGRAM` |
| `dealSource` | string | Deal source (Direct, Divert, Reference, Planner, TBS) | `dealSource=Direct` |
| `organizationId` | string | Comma-separated organization IDs | `organizationId=1,2,3` |
| `ownerId` | string | Comma-separated owner/user IDs | `ownerId=5,6` |
| `categoryId` | string | Comma-separated category IDs | `categoryId=1,2` |
| `leadFrom` | date | Created date from (format: `dd/MM/yyyy`) - filters by `created_at` field | `leadFrom=01/01/2025` |
| `leadTo` | date | Created date to (format: `dd/MM/yyyy`) - filters by `created_at` field | `leadTo=31/12/2025` |
| `page` | integer | Page number (0-based, default: 0) | `page=0` |
| `size` | integer | Page size (default: 200, max recommended: 200) | `size=200` |
| `sort` | string | Sort field and direction (default: `createdAt,DESC`) | `sort=name,ASC` |

### Response Structure

```json
{
  "persons": [
    {
      "id": 101,
      "name": "John Doe",
      "instagramId": "@johndoe",
      "phone": "+1234567890",
      "email": "john@example.com",
      "leadDate": "2025-01-15",
      "venue": "The Taj Palace",
      "eventDate": "2025-12-19",
      "organizationId": 12,
      "organizationName": "Wedding Planners Inc",
      "ownerId": 5,
      "ownerDisplayName": "Rohan Gupta",
      "ownerEmail": "rohan@brideside.com",
      "categoryId": 3,
      "categoryName": "Bridal Makeup",
      "labelId": 2,
      "label": {
        "id": 2,
        "name": "Bridal Makeup",
        "color": "#FF5733",
        "createdAt": "2025-01-01T00:00:00Z",
        "updatedAt": "2025-01-01T00:00:00Z"
      },
      "labelEnum": null,
      "source": "INSTAGRAM",
      "subSource": "WHATSAPP",
      "createdAt": "2025-01-15T10:30:00Z",
      "updatedAt": "2025-01-20T14:22:00Z"
    }
  ],
  "deals": [
    {
      "id": 215,
      "name": "Wedding Package - Doe",
      "value": 125000,
      "personId": 101,
      "personName": "John Doe",
      "pipelineId": 8,
      "stageId": 37,
      "sourceId": 4,
      "organizationId": 12,
      "organizationName": "Wedding Planners Inc",
      "categoryId": 3,
      "eventType": "Mehendi",
      "status": "IN_PROGRESS",
      "commissionAmount": 7500,
      "createdAt": "2025-11-12T07:05:41.114Z",
      "venue": "The Taj Palace",
      "phoneNumber": "+1234567890",
      "finalThankYouSent": false,
      "eventDateAsked": true,
      "contactNumberAsked": true,
      "venueAsked": false,
      "eventDate": "2025-12-19",
      "eventDates": ["2025-12-19"],
      "labelId": 1,
      "label": {
        "id": 1,
        "name": "Direct",
        "color": "#4CAF50"
      },
      "labelIds": [1],
      "labels": [
        {
          "id": 1,
          "name": "Direct",
          "color": "#4CAF50"
        }
      ],
      "source": "Direct",
      "subSource": "Instagram",
      "isDiverted": false
    }
  ],
  "activities": [
    {
      "id": 42,
      "subject": "Follow-up call",
      "type": "CALL",
      "category": "CALL",
      "date": "2025-01-20",
      "dueDate": "2025-01-20",
      "startTime": "14:00",
      "endTime": "14:30",
      "priority": "HIGH",
      "assignedUser": "rohan@brideside.com",
      "notes": "Discuss wedding package options",
      "personId": 101,
      "dealId": 215,
      "dealName": "Wedding Package - Doe",
      "organization": "Wedding Planners Inc",
      "done": false,
      "status": "PENDING",
      "callType": "OUTBOUND",
      "instagramId": "@johndoe",
      "phone": "+1234567890"
    }
  ],
  "pagination": {
    "page": 0,
    "size": 200,
    "totalElements": 450,
    "totalPages": 3,
    "hasNext": true,
    "hasPrevious": false
  },
  "totalPersons": 450
}
```

### Response Fields Description

**Top-level fields:**
- `persons` (array): List of PersonDTO objects matching the filters
- `deals` (array): All deals associated with the persons in this page (via `person_id`)
- `activities` (array): All activities associated with the persons in this page (via `personId`)
- `pagination` (object): Pagination metadata
- `totalPersons` (number): Total number of persons matching the selected filters and date range (regardless of pagination)

**Important Notes:**
- `deals` array contains ALL deals for ALL persons in the current page (not just the first person)
- `activities` array contains ALL activities for ALL persons in the current page
- To associate deals/activities with persons, use `personId` field in deals/activities
- The response uses JOINs internally, so it's efficient even with large datasets

---

## Frontend Implementation

### Step 1: Replace Current API Calls

**Before (Current Implementation):**
```javascript
// ❌ DON'T DO THIS ANYMORE
const [deals, setDeals] = useState([]);
const [activities, setActivities] = useState([]);

useEffect(() => {
  // This loads ALL deals - very slow!
  fetch('/api/deals')
    .then(res => res.json())
    .then(data => setDeals(data.content || data));
    
  // This may load unrelated activities
  fetch('/api/activities')
    .then(res => res.json())
    .then(data => setActivities(data.content || data));
}, []);
```

**After (New Implementation):**
```javascript
// ✅ NEW OPTIMIZED APPROACH
const [persons, setPersons] = useState([]);
const [deals, setDeals] = useState([]);
const [activities, setActivities] = useState([]);
const [loading, setLoading] = useState(false);
const [hasMore, setHasMore] = useState(true);
const [currentPage, setCurrentPage] = useState(0);

const PAGE_SIZE = 200;

const loadPersonsWithDetails = async (page = 0, filters = {}) => {
  setLoading(true);
  try {
    const params = new URLSearchParams({
      page: page.toString(),
      size: PAGE_SIZE.toString(),
      sort: 'createdAt,DESC',
      ...filters // Add your filter parameters
    });
    
    const response = await fetch(`/api/persons/with-details?${params}`);
    const data = await response.json();
    
    if (page === 0) {
      // First page - replace data
      setPersons(data.persons);
      setDeals(data.deals);
      setActivities(data.activities);
    } else {
      // Subsequent pages - append data
      setPersons(prev => [...prev, ...data.persons]);
      setDeals(prev => [...prev, ...data.deals]);
      setActivities(prev => [...prev, ...data.activities]);
    }
    
    setHasMore(data.pagination.hasNext);
    setCurrentPage(page);
  } catch (error) {
    console.error('Error loading persons:', error);
  } finally {
    setLoading(false);
  }
};

// Load initial data
useEffect(() => {
  loadPersonsWithDetails(0, {
    leadFrom: '01/01/2025',
    leadTo: '31/12/2025',
    // Add other filters from your UI
  });
}, []); // Add filter dependencies as needed
```

### Step 2: Implement Infinite Scroll

**Using Intersection Observer (Recommended):**

```javascript
import { useEffect, useRef, useCallback } from 'react';

const InfiniteScrollPersons = () => {
  const [persons, setPersons] = useState([]);
  const [deals, setDeals] = useState([]);
  const [activities, setActivities] = useState([]);
  const [loading, setLoading] = useState(false);
  const [hasMore, setHasMore] = useState(true);
  const [currentPage, setCurrentPage] = useState(0);
  const [filters, setFilters] = useState({});
  
  const observerTarget = useRef(null);
  const PAGE_SIZE = 200;

  const loadPersonsWithDetails = useCallback(async (page = 0, appliedFilters = {}) => {
    if (loading) return;
    
    setLoading(true);
    try {
      const params = new URLSearchParams({
        page: page.toString(),
        size: PAGE_SIZE.toString(),
        sort: 'createdAt,DESC',
        ...appliedFilters
      });
      
      const response = await fetch(`/api/persons/with-details?${params}`);
      const data = await response.json();
      
      if (page === 0) {
        // First page - replace all data
        setPersons(data.persons);
        setDeals(data.deals);
        setActivities(data.activities);
      } else {
        // Append new data
        setPersons(prev => [...prev, ...data.persons]);
        setDeals(prev => {
          // Merge deals, avoiding duplicates
          const existingIds = new Set(prev.map(d => d.id));
          const newDeals = data.deals.filter(d => !existingIds.has(d.id));
          return [...prev, ...newDeals];
        });
        setActivities(prev => {
          // Merge activities, avoiding duplicates
          const existingIds = new Set(prev.map(a => a.id));
          const newActivities = data.activities.filter(a => !existingIds.has(a.id));
          return [...prev, ...newActivities];
        });
      }
      
      setHasMore(data.pagination.hasNext);
      setCurrentPage(page);
    } catch (error) {
      console.error('Error loading persons:', error);
    } finally {
      setLoading(false);
    }
  }, [loading]);

  // Load initial data
  useEffect(() => {
    loadPersonsWithDetails(0, filters);
  }, []); // Only on mount

  // Reload when filters change
  useEffect(() => {
    setCurrentPage(0);
    setHasMore(true);
    loadPersonsWithDetails(0, filters);
  }, [filters]); // Reload when filters change

  // Infinite scroll observer
  useEffect(() => {
    const observer = new IntersectionObserver(
      entries => {
        if (entries[0].isIntersecting && hasMore && !loading) {
          loadPersonsWithDetails(currentPage + 1, filters);
        }
      },
      { threshold: 0.1 }
    );

    if (observerTarget.current) {
      observer.observe(observerTarget.current);
    }

    return () => {
      if (observerTarget.current) {
        observer.unobserve(observerTarget.current);
      }
    };
  }, [hasMore, loading, currentPage, filters, loadPersonsWithDetails]);

  // Helper function to get deals for a specific person
  const getDealsForPerson = useCallback((personId) => {
    return deals.filter(d => d.personId === personId);
  }, [deals]);

  // Helper function to get activities for a specific person
  const getActivitiesForPerson = useCallback((personId) => {
    return activities.filter(a => a.personId === personId);
  }, [activities]);

  return (
    <div>
      {/* Your filter UI */}
      <FilterPanel 
        filters={filters} 
        onChange={setFilters}
      />

      {/* Persons list */}
      <div>
        {persons.map(person => (
          <PersonCard
            key={person.id}
            person={person}
            deals={getDealsForPerson(person.id)}
            activities={getActivitiesForPerson(person.id)}
          />
        ))}
      </div>

      {/* Infinite scroll trigger */}
      {hasMore && (
        <div ref={observerTarget} style={{ height: '20px' }}>
          {loading && <div>Loading more...</div>}
        </div>
      )}

      {!hasMore && persons.length > 0 && (
        <div>No more persons to load</div>
      )}
    </div>
  );
};
```

### Step 3: Handle Filter Changes

```javascript
const handleFilterChange = (newFilters) => {
  // Reset pagination when filters change
  setCurrentPage(0);
  setHasMore(true);
  setPersons([]);
  setDeals([]);
  setActivities([]);
  
  // Apply new filters
  setFilters(newFilters);
  // The useEffect will automatically reload data
};
```

### Step 4: Map Data for Person Details Page

```javascript
// When displaying a person detail page, use the loaded data
const PersonDetailsPage = ({ personId }) => {
  const person = persons.find(p => p.id === personId);
  const personDeals = deals.filter(d => d.personId === personId);
  const personActivities = activities.filter(a => a.personId === personId);

  if (!person) {
    // Person not in loaded set - fetch individually if needed
    // Or preload more data
    return <div>Loading person details...</div>;
  }

  return (
    <div>
      <PersonInfo person={person} />
      <DealsList deals={personDeals} />
      <ActivitiesList activities={personActivities} />
    </div>
  );
};
```

---

## Migration Checklist

### Backend (Already Implemented)
- [x] Create `/api/persons/with-details` endpoint
- [x] Implement JOIN-based queries for deals and activities
- [x] Support all existing person filters
- [x] Add pagination support (200 per page)
- [x] Return structured response with persons, deals, activities, and pagination

### Frontend (To Be Implemented)
- [ ] Replace `/api/deals` calls with `/api/persons/with-details`
- [ ] Remove separate `/api/activities` calls
- [ ] Implement infinite scroll using Intersection Observer
- [ ] Handle filter changes and reset pagination
- [ ] Map deals and activities to persons using `personId`
- [ ] Update PersonDetailsPage to use loaded data
- [ ] Add loading states and error handling
- [ ] Test with various filter combinations
- [ ] Test infinite scroll behavior

---

## Performance Benefits

**Before:**
- Multiple API calls (`/api/deals`, `/api/activities`)
- Loading ALL deals (potentially thousands)
- Loading potentially unrelated activities
- Client-side filtering and mapping
- Slow initial page load

**After:**
- Single API call
- Only loads persons matching filters (200 per page)
- Only loads deals/activities for those persons
- Server-side JOINs (efficient database queries)
- Fast initial page load
- Progressive loading with infinite scroll

**Expected Improvements:**
- **Initial Load Time:** 70-90% faster
- **Data Transfer:** 80-95% reduction
- **Server Load:** Significantly reduced
- **User Experience:** Smooth infinite scroll, no page freezing

---

## Error Handling

```javascript
const loadPersonsWithDetails = async (page = 0, filters = {}) => {
  setLoading(true);
  setError(null);
  
  try {
    const params = new URLSearchParams({
      page: page.toString(),
      size: PAGE_SIZE.toString(),
      sort: 'createdAt,DESC',
      ...filters
    });
    
    const response = await fetch(`/api/persons/with-details?${params}`);
    
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    
    const data = await response.json();
    
    // Process data...
    
  } catch (error) {
    console.error('Error loading persons:', error);
    setError(error.message);
    // Show user-friendly error message
  } finally {
    setLoading(false);
  }
};
```

---

## Testing Recommendations

1. **Load Test:** Test with large datasets (1000+ persons)
2. **Filter Test:** Test all filter combinations
3. **Scroll Test:** Test infinite scroll with various page sizes
4. **Performance Test:** Measure load times before/after
5. **Network Test:** Test with slow network conditions
6. **Edge Cases:**
   - Empty results
   - Single page of results
   - Very large page numbers
   - Invalid filters
   - Network errors

---

## API Example Requests

### Basic Request (First Page)
```
GET /api/persons/with-details?page=0&size=200&sort=createdAt,DESC
```

### With Date Range Filter (by created_at)
```
GET /api/persons/with-details?page=0&size=200&leadFrom=01/01/2025&leadTo=31/12/2025
```
**Note:** The date range filters by `created_at` field (when the person record was created), not by `lead_date`.

### With Multiple Filters
```
GET /api/persons/with-details?page=0&size=200&label=BRIDAL_MAKEUP&organizationId=1,2,3&ownerId=5&leadFrom=01/01/2025&leadTo=31/12/2025
```

### Search Query
```
GET /api/persons/with-details?page=0&size=200&q=john&sort=name,ASC
```

### Next Page (Infinite Scroll)
```
GET /api/persons/with-details?page=1&size=200&leadFrom=01/01/2025&leadTo=31/12/2025
```

---

## Notes

1. **Page Size:** Default is 200. You can adjust based on your needs, but larger pages may impact performance.

2. **Data Deduplication:** The frontend should handle merging deals/activities when loading new pages, as some items might appear in multiple pages if a person has many related records.

3. **Filter Persistence:** Store filter state in URL query parameters or localStorage for better UX.

4. **Cache Strategy:** Consider implementing client-side caching for recently loaded pages to improve navigation experience.

5. **Real-time Updates:** If persons/deals/activities are updated elsewhere, you may need to refetch or implement real-time updates via WebSockets or polling.

---

## Support

For questions or issues, please refer to:
- Backend API documentation: Check Swagger UI at `/swagger-ui.html`
- Deal API documentation: `DEAL_DETAILS_API_OPTIMIZATION_FRONTEND.md`
- Person API documentation: `frontend-persons-api.md`

