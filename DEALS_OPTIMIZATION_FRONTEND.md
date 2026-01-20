# Deals API Optimization - Frontend Changes

## Overview

The `/api/deals` endpoint has been optimized to automatically include related `Person` and `Activity` data in the response. This eliminates the need for separate API calls to fetch persons and activities, significantly reducing page load time and optimizing the application for large datasets.

## Backend Changes

### API Response Structure

The `/api/deals` endpoint now returns a response with the following structure:

```json
{
  "deals": [
    {
      "id": 1,
      "name": "Wedding Deal",
      "personId": 123,
      "personName": "John Doe",
      // ... other deal fields
    }
  ],
  "totalCount": 300,
  "persons": [
    {
      "id": 123,
      "name": "John Doe",
      "phone": "+1234567890",
      "email": "john@example.com",
      // ... other person fields
    }
  ],
  "activities": [
    {
      "id": 456,
      "subject": "Follow up call",
      "dealId": 1,
      "dealName": "Wedding Deal",
      // ... other activity fields
    }
  ]
}
```

### Key Points

1. **Automatic Data Inclusion**: The backend automatically extracts deal IDs from the loaded deals and fetches only the related persons and activities using efficient JOIN queries.

2. **Optimized Queries**: 
   - Persons are fetched using JOINs with their related entities (organization, owner, category, label) to avoid N+1 queries
   - Activities are fetched using JOINs with their related entities (organization) to avoid N+1 queries

3. **No Separate API Calls Needed**: The frontend no longer needs to make separate calls to `/api/persons` and `/api/activities` endpoints.

## Frontend Changes Required

### 1. Update API Response Type

Update the TypeScript interface/type for the deals API response:

```typescript
interface DealListResponse {
  deals: DealResponse[];
  totalCount: number;
  persons: PersonDTO[];      // NEW: All persons linked to the returned deals
  activities: ActivityDTO[];  // NEW: All activities linked to the returned deals
}
```

### 2. Remove Separate API Calls

**Before:**
```typescript
// OLD APPROACH - Remove these calls
const dealsResponse = await fetch('/api/deals?pipelineId=44&status=IN_PROGRESS&limit=100&offset=0');
const deals = await dealsResponse.json();

// Separate API calls (REMOVE THESE)
const personsResponse = await fetch('/api/persons?page=0&size=50000&sort=name,asc');
const persons = await personsResponse.json();

const activitiesResponse = await fetch('/api/activities?page=0&size=500');
const activities = await activitiesResponse.json();
```

**After:**
```typescript
// NEW APPROACH - Single API call
const response = await fetch('/api/deals?pipelineId=44&status=IN_PROGRESS&limit=100&offset=0');
const data: DealListResponse = await response.json();

// Use data directly from the response
const deals = data.deals;
const persons = data.persons;      // Already included
const activities = data.activities; // Already included
```

### 3. Update State Management

If you're using state management (Redux, Context, etc.), update your state structure:

```typescript
// Example with React Context or Redux
interface DealsState {
  deals: DealResponse[];
  totalCount: number;
  persons: PersonDTO[];      // NEW
  activities: ActivityDTO[];  // NEW
}

// When fetching deals
const fetchDeals = async (filters: DealFilters) => {
  const response = await fetch(`/api/deals?${buildQueryString(filters)}`);
  const data: DealListResponse = await response.json();
  
  setDealsState({
    deals: data.deals,
    totalCount: data.totalCount,
    persons: data.persons,      // NEW
    activities: data.activities  // NEW
  });
};
```

### 4. Update Person Lookup Logic

Update any code that looks up person data by personId:

**Before:**
```typescript
// OLD: Lookup from separate persons array
const getPersonForDeal = (deal: DealResponse) => {
  return persons.find(p => p.id === deal.personId);
};
```

**After:**
```typescript
// NEW: Lookup from persons included in deals response
const getPersonForDeal = (deal: DealResponse, persons: PersonDTO[]) => {
  return persons.find(p => p.id === deal.personId);
};

// Or create a lookup map for better performance
const createPersonMap = (persons: PersonDTO[]) => {
  return new Map(persons.map(p => [p.id, p]));
};

const personMap = createPersonMap(data.persons);
const person = personMap.get(deal.personId);
```

### 5. Update Activity Lookup Logic

Update any code that looks up activity data by dealId:

**Before:**
```typescript
// OLD: Lookup from separate activities array
const getActivitiesForDeal = (dealId: number) => {
  return activities.filter(a => a.dealId === dealId);
};
```

**After:**
```typescript
// NEW: Lookup from activities included in deals response
const getActivitiesForDeal = (dealId: number, activities: ActivityDTO[]) => {
  return activities.filter(a => a.dealId === dealId);
};

// Or create a lookup map for better performance
const createActivityMap = (activities: ActivityDTO[]) => {
  const map = new Map<number, ActivityDTO[]>();
  activities.forEach(activity => {
    if (activity.dealId) {
      if (!map.has(activity.dealId)) {
        map.set(activity.dealId, []);
      }
      map.get(activity.dealId)!.push(activity);
    }
  });
  return map;
};

const activityMap = createActivityMap(data.activities);
const dealActivities = activityMap.get(deal.id) || [];
```

### 6. Handle Pagination

When loading more deals (e.g., on scroll), merge the new persons and activities with existing ones:

```typescript
const loadMoreDeals = async (offset: number) => {
  const response = await fetch(`/api/deals?${filters}&limit=100&offset=${offset}`);
  const newData: DealListResponse = await response.json();
  
  // Merge deals
  setDeals(prev => [...prev, ...newData.deals]);
  
  // Merge persons (avoid duplicates by ID)
  setPersons(prev => {
    const personMap = new Map(prev.map(p => [p.id, p]));
    newData.persons.forEach(p => personMap.set(p.id, p));
    return Array.from(personMap.values());
  });
  
  // Merge activities (avoid duplicates by ID)
  setActivities(prev => {
    const activityMap = new Map(prev.map(a => [a.id, a]));
    newData.activities.forEach(a => activityMap.set(a.id, a));
    return Array.from(activityMap.values());
  });
};
```

### 7. Update Component Props

If you're passing persons and activities as props, update your components:

```typescript
// Component that displays deal with person and activity info
interface DealCardProps {
  deal: DealResponse;
  person?: PersonDTO;        // Now comes from deals response
  activities: ActivityDTO[];  // Now comes from deals response
}

const DealCard: React.FC<DealCardProps> = ({ deal, person, activities }) => {
  const dealActivities = activities.filter(a => a.dealId === deal.id);
  
  return (
    <div>
      <h3>{deal.name}</h3>
      {person && <p>Person: {person.name}</p>}
      <p>Activities: {dealActivities.length}</p>
      {/* ... */}
    </div>
  );
};
```

## Benefits

1. **Reduced API Calls**: From 3 separate API calls to 1 single call
2. **Faster Page Load**: No need to wait for multiple API responses
3. **Optimized Data**: Only fetches persons and activities relevant to the loaded deals
4. **Better Performance**: Especially noticeable with large datasets (lakhs of persons/activities)
5. **Simplified Frontend Logic**: No need to extract deal IDs and make additional API calls

## Migration Checklist

- [ ] Update `DealListResponse` TypeScript interface to include `persons` and `activities`
- [ ] Remove separate API calls to `/api/persons` and `/api/activities` in the deals page
- [ ] Update state management to use persons and activities from deals response
- [ ] Update person lookup logic to use persons from deals response
- [ ] Update activity lookup logic to use activities from deals response
- [ ] Update pagination logic to merge new persons and activities
- [ ] Update component props/types to reflect new data structure
- [ ] Test with different filters and pagination scenarios
- [ ] Verify performance improvements with large datasets

## Example: Complete Implementation

```typescript
// types.ts
export interface DealListResponse {
  deals: DealResponse[];
  totalCount: number;
  persons: PersonDTO[];
  activities: ActivityDTO[];
}

// hooks/useDeals.ts
export const useDeals = (filters: DealFilters) => {
  const [deals, setDeals] = useState<DealResponse[]>([]);
  const [persons, setPersons] = useState<PersonDTO[]>([]);
  const [activities, setActivities] = useState<ActivityDTO[]>([]);
  const [totalCount, setTotalCount] = useState(0);
  const [loading, setLoading] = useState(false);

  const fetchDeals = async (offset = 0) => {
    setLoading(true);
    try {
      const queryParams = new URLSearchParams({
        ...filters,
        limit: '100',
        offset: offset.toString(),
      });
      
      const response = await fetch(`/api/deals?${queryParams}`);
      const data: DealListResponse = await response.json();
      
      if (offset === 0) {
        // First page - replace all data
        setDeals(data.deals);
        setPersons(data.persons);
        setActivities(data.activities);
      } else {
        // Subsequent pages - merge data
        setDeals(prev => [...prev, ...data.deals]);
        
        // Merge persons (avoid duplicates)
        setPersons(prev => {
          const map = new Map(prev.map(p => [p.id, p]));
          data.persons.forEach(p => map.set(p.id, p));
          return Array.from(map.values());
        });
        
        // Merge activities (avoid duplicates)
        setActivities(prev => {
          const map = new Map(prev.map(a => [a.id, a]));
          data.activities.forEach(a => map.set(a.id, a));
          return Array.from(map.values());
        });
      }
      
      setTotalCount(data.totalCount);
    } catch (error) {
      console.error('Failed to fetch deals:', error);
    } finally {
      setLoading(false);
    }
  };

  // Create lookup maps for efficient access
  const personMap = useMemo(
    () => new Map(persons.map(p => [p.id, p])),
    [persons]
  );

  const activityMap = useMemo(() => {
    const map = new Map<number, ActivityDTO[]>();
    activities.forEach(activity => {
      if (activity.dealId) {
        if (!map.has(activity.dealId)) {
          map.set(activity.dealId, []);
        }
        map.get(activity.dealId)!.push(activity);
      }
    });
    return map;
  }, [activities]);

  return {
    deals,
    persons,
    activities,
    totalCount,
    loading,
    fetchDeals,
    personMap,
    activityMap,
  };
};

// components/DealCard.tsx
interface DealCardProps {
  deal: DealResponse;
  person?: PersonDTO;
  activities: ActivityDTO[];
}

export const DealCard: React.FC<DealCardProps> = ({ deal, person, activities }) => {
  const dealActivities = activities.filter(a => a.dealId === deal.id);

  return (
    <div className="deal-card">
      <h3>{deal.name}</h3>
      {person && (
        <div>
          <p>Person: {person.name}</p>
          <p>Phone: {person.phone}</p>
          <p>Email: {person.email}</p>
        </div>
      )}
      <div>
        <h4>Activities ({dealActivities.length})</h4>
        {dealActivities.map(activity => (
          <div key={activity.id}>
            <p>{activity.subject}</p>
            <p>Status: {activity.status}</p>
          </div>
        ))}
      </div>
    </div>
  );
};

// pages/DealsPage.tsx
export const DealsPage: React.FC = () => {
  const filters = useDealFilters(); // Your filter hook
  const { deals, persons, activities, totalCount, loading, fetchDeals, personMap, activityMap } = useDeals(filters);

  useEffect(() => {
    fetchDeals(0);
  }, [filters]);

  const handleLoadMore = () => {
    fetchDeals(deals.length);
  };

  return (
    <div>
      <h1>Deals ({totalCount})</h1>
      {deals.map(deal => (
        <DealCard
          key={deal.id}
          deal={deal}
          person={personMap.get(deal.personId)}
          activities={activityMap.get(deal.id) || []}
        />
      ))}
      {deals.length < totalCount && (
        <button onClick={handleLoadMore} disabled={loading}>
          {loading ? 'Loading...' : 'Load More'}
        </button>
      )}
    </div>
  );
};
```

## Notes

- The backend automatically handles deduplication of persons and activities
- Persons are fetched with all their related data (organization, owner, category, label) using JOINs
- Activities are fetched with their related organization data using JOINs
- The response only includes persons and activities that are actually linked to the returned deals
- When paginating, you may want to merge new persons/activities with existing ones to avoid duplicates
