# Deals Sorting - Frontend Integration Guide

This guide provides step-by-step instructions for integrating the deals sorting API into your frontend application.

## Overview

The `GET /api/deals` endpoint now supports sorting via a `sort` query parameter. The format is `field,direction` where:
- `field` is the field to sort by (see supported fields below)
- `direction` is either `asc` (ascending) or `desc` (descending)
- Default: `nextActivity,asc` (if not specified)

## API Endpoint

```
GET /api/deals?sort={field},{direction}
```

## Supported Sort Fields

| Frontend Display | API Sort Field | Aliases | Type |
|-----------------|----------------|---------|------|
| Next activity | `nextActivity` | - | DateTime |
| Deal title | `name` | `dealTitle` | String |
| Deal value | `value` | `dealValue` | Number |
| Linked person | `personName` | `linkedPerson` | String |
| Linked organization | `organizationName` | `linkedOrganization` | String |
| Expected close date | `eventDate` | `expectedCloseDate` | Date |
| Deal created | `createdAt` | `dealCreated` | DateTime |
| Deal update time | `updatedAt` | `dealUpdateTime` | DateTime |
| Done activities | `completedActivitiesCount` | `doneActivities` | Number |
| Activities to do | `pendingActivitiesCount` | `activitiesToDo` | Number |
| Number of products | `productsCount` | `numberOfProducts` | Number |
| Owner name | `ownerName` | `personOwnerName` | String |

## Frontend Implementation

### Step 1: Update Your API Service/Client

Update your deals API call to accept and pass the sort parameter:

```typescript
// Example: Using fetch
async function fetchDeals(sortField: string = 'nextActivity', sortDirection: 'asc' | 'desc' = 'asc') {
  const response = await fetch(`/api/deals?sort=${sortField},${sortDirection}`, {
    headers: {
      'Authorization': `Bearer ${token}`,
      'Content-Type': 'application/json'
    }
  });
  
  if (!response.ok) {
    throw new Error('Failed to fetch deals');
  }
  
  return response.json();
}

// Example: Using axios
import axios from 'axios';

async function fetchDeals(sortField: string = 'nextActivity', sortDirection: 'asc' | 'desc' = 'asc') {
  const response = await axios.get('/api/deals', {
    params: {
      sort: `${sortField},${sortDirection}`
    },
    headers: {
      'Authorization': `Bearer ${token}`
    }
  });
  
  return response.data;
}
```

### Step 2: Create Sort Dropdown Component

Create a dropdown component for selecting sort field and direction:

```tsx
// SortDropdown.tsx
import React, { useState } from 'react';

interface SortOption {
  value: string;
  label: string;
  aliases?: string[];
}

const SORT_OPTIONS: SortOption[] = [
  { value: 'nextActivity', label: 'Next activity' },
  { value: 'name', label: 'Deal title', aliases: ['dealTitle'] },
  { value: 'value', label: 'Deal value', aliases: ['dealValue'] },
  { value: 'personName', label: 'Linked person', aliases: ['linkedPerson'] },
  { value: 'organizationName', label: 'Linked organization', aliases: ['linkedOrganization'] },
  { value: 'eventDate', label: 'Expected close date', aliases: ['expectedCloseDate'] },
  { value: 'createdAt', label: 'Deal created', aliases: ['dealCreated'] },
  { value: 'updatedAt', label: 'Deal update time', aliases: ['dealUpdateTime'] },
  { value: 'completedActivitiesCount', label: 'Done activities', aliases: ['doneActivities'] },
  { value: 'pendingActivitiesCount', label: 'Activities to do', aliases: ['activitiesToDo'] },
  { value: 'productsCount', label: 'Number of products', aliases: ['numberOfProducts'] },
  { value: 'ownerName', label: 'Owner name', aliases: ['personOwnerName'] }
];

interface SortDropdownProps {
  onSortChange: (field: string, direction: 'asc' | 'desc') => void;
  currentField?: string;
  currentDirection?: 'asc' | 'desc';
}

export const SortDropdown: React.FC<SortDropdownProps> = ({
  onSortChange,
  currentField = 'nextActivity',
  currentDirection = 'asc'
}) => {
  const [field, setField] = useState(currentField);
  const [direction, setDirection] = useState<'asc' | 'desc'>(currentDirection);

  const handleFieldChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const newField = e.target.value;
    setField(newField);
    onSortChange(newField, direction);
  };

  const handleDirectionChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const newDirection = e.target.value as 'asc' | 'desc';
    setDirection(newDirection);
    onSortChange(field, newDirection);
  };

  return (
    <div className="sort-dropdown">
      <label htmlFor="sort-field">Sort by:</label>
      <select
        id="sort-field"
        value={field}
        onChange={handleFieldChange}
      >
        {SORT_OPTIONS.map(option => (
          <option key={option.value} value={option.value}>
            {option.label}
          </option>
        ))}
      </select>

      <label htmlFor="sort-direction">Direction:</label>
      <select
        id="sort-direction"
        value={direction}
        onChange={handleDirectionChange}
      >
        <option value="asc">Ascending</option>
        <option value="desc">Descending</option>
      </select>
    </div>
  );
};
```

### Step 3: Integrate into Deals List Component

Update your deals list component to use sorting:

```tsx
// DealsList.tsx
import React, { useState, useEffect } from 'react';
import { SortDropdown } from './SortDropdown';
import { fetchDeals } from './api/deals';

interface Deal {
  id: number;
  name: string;
  value: number;
  // ... other deal fields
}

export const DealsList: React.FC = () => {
  const [deals, setDeals] = useState<Deal[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [sortField, setSortField] = useState('nextActivity');
  const [sortDirection, setSortDirection] = useState<'asc' | 'desc'>('asc');

  useEffect(() => {
    loadDeals();
  }, [sortField, sortDirection]);

  const loadDeals = async () => {
    setLoading(true);
    setError(null);
    
    try {
      const data = await fetchDeals(sortField, sortDirection);
      setDeals(data);
    } catch (err) {
      setError('Failed to load deals');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleSortChange = (field: string, direction: 'asc' | 'desc') => {
    setSortField(field);
    setSortDirection(direction);
  };

  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error: {error}</div>;

  return (
    <div>
      <SortDropdown
        onSortChange={handleSortChange}
        currentField={sortField}
        currentDirection={sortDirection}
      />
      
      <table>
        <thead>
          <tr>
            <th>Name</th>
            <th>Value</th>
            <th>Status</th>
            {/* ... other columns */}
          </tr>
        </thead>
        <tbody>
          {deals.map(deal => (
            <tr key={deal.id}>
              <td>{deal.name}</td>
              <td>{deal.value}</td>
              <td>{deal.status}</td>
              {/* ... other cells */}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};
```

### Step 4: Alternative - Single Dropdown with Combined Options

If you prefer a single dropdown with combined options:

```tsx
// CombinedSortDropdown.tsx
import React, { useState } from 'react';

const SORT_OPTIONS = [
  { value: 'nextActivity,asc', label: 'Next activity (Ascending)' },
  { value: 'nextActivity,desc', label: 'Next activity (Descending)' },
  { value: 'name,asc', label: 'Deal title (A-Z)' },
  { value: 'name,desc', label: 'Deal title (Z-A)' },
  { value: 'value,asc', label: 'Deal value (Low to High)' },
  { value: 'value,desc', label: 'Deal value (High to Low)' },
  { value: 'personName,asc', label: 'Person name (A-Z)' },
  { value: 'personName,desc', label: 'Person name (Z-A)' },
  { value: 'organizationName,asc', label: 'Organization (A-Z)' },
  { value: 'organizationName,desc', label: 'Organization (Z-A)' },
  { value: 'eventDate,asc', label: 'Event date (Earliest first)' },
  { value: 'eventDate,desc', label: 'Event date (Latest first)' },
  { value: 'createdAt,asc', label: 'Created (Oldest first)' },
  { value: 'createdAt,desc', label: 'Created (Newest first)' },
  { value: 'updatedAt,asc', label: 'Updated (Oldest first)' },
  { value: 'updatedAt,desc', label: 'Updated (Newest first)' },
  { value: 'completedActivitiesCount,asc', label: 'Done activities (Fewest first)' },
  { value: 'completedActivitiesCount,desc', label: 'Done activities (Most first)' },
  { value: 'pendingActivitiesCount,asc', label: 'Activities to do (Fewest first)' },
  { value: 'pendingActivitiesCount,desc', label: 'Activities to do (Most first)' },
  { value: 'ownerName,asc', label: 'Owner name (A-Z)' },
  { value: 'ownerName,desc', label: 'Owner name (Z-A)' }
];

interface CombinedSortDropdownProps {
  onSortChange: (sortValue: string) => void;
  currentValue?: string;
}

export const CombinedSortDropdown: React.FC<CombinedSortDropdownProps> = ({
  onSortChange,
  currentValue = 'nextActivity,asc'
}) => {
  const handleChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    onSortChange(e.target.value);
  };

  return (
    <div className="sort-dropdown">
      <label htmlFor="sort">Sort by:</label>
      <select
        id="sort"
        value={currentValue}
        onChange={handleChange}
      >
        {SORT_OPTIONS.map(option => (
          <option key={option.value} value={option.value}>
            {option.label}
          </option>
        ))}
      </select>
    </div>
  );
};

// Usage in DealsList component:
const [sort, setSort] = useState('nextActivity,asc');

const handleSortChange = (sortValue: string) => {
  setSort(sortValue);
  const [field, direction] = sortValue.split(',');
  loadDeals(field, direction as 'asc' | 'desc');
};
```

## Error Handling

Handle API errors appropriately:

```typescript
async function fetchDeals(sortField: string, sortDirection: 'asc' | 'desc') {
  try {
    const response = await fetch(`/api/deals?sort=${sortField},${sortDirection}`, {
      headers: {
        'Authorization': `Bearer ${token}`,
        'Content-Type': 'application/json'
      }
    });

    if (!response.ok) {
      if (response.status === 400) {
        const error = await response.json();
        throw new Error(error.message || 'Invalid sort parameters');
      }
      throw new Error('Failed to fetch deals');
    }

    return response.json();
  } catch (error) {
    console.error('Error fetching deals:', error);
    throw error;
  }
}
```

## Example: React Hook for Deals with Sorting

```tsx
// useDeals.ts
import { useState, useEffect } from 'react';

interface UseDealsOptions {
  sortField?: string;
  sortDirection?: 'asc' | 'desc';
}

export function useDeals(options: UseDealsOptions = {}) {
  const { sortField = 'nextActivity', sortDirection = 'asc' } = options;
  const [deals, setDeals] = useState([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    setLoading(true);
    setError(null);

    fetchDeals(sortField, sortDirection)
      .then(data => {
        setDeals(data);
        setLoading(false);
      })
      .catch(err => {
        setError(err.message);
        setLoading(false);
      });
  }, [sortField, sortDirection]);

  return { deals, loading, error };
}

// Usage:
const { deals, loading, error } = useDeals({
  sortField: 'value',
  sortDirection: 'desc'
});
```

## Testing

Test the following scenarios:

1. **Default sorting**: Call `/api/deals` without sort parameter (should default to `nextActivity,asc`)
2. **All sort fields**: Test each sort field with both ascending and descending
3. **Invalid sort field**: Test with invalid field (should return 400 error)
4. **Invalid direction**: Test with invalid direction (should default to `asc`)
5. **Null values**: Verify deals with null values are handled correctly (sorted to end/beginning)

## Quick Reference

### Common Sort Combinations

```javascript
// Most recent updates first
GET /api/deals?sort=updatedAt,desc

// Highest value deals first
GET /api/deals?sort=value,desc

// Alphabetical by deal name
GET /api/deals?sort=name,asc

// Next activity soonest first
GET /api/deals?sort=nextActivity,asc

// Most pending activities first
GET /api/deals?sort=pendingActivitiesCount,desc
```

### Field Mapping for Your Dropdown

When creating your dropdown, you can use either the canonical field names or aliases:

- `name` or `dealTitle` → "Deal title"
- `value` or `dealValue` → "Deal value"
- `personName` or `linkedPerson` → "Linked person"
- `organizationName` or `linkedOrganization` → "Linked organization"
- `eventDate` or `expectedCloseDate` → "Expected close date"
- `createdAt` or `dealCreated` → "Deal created"
- `updatedAt` or `dealUpdateTime` → "Deal update time"
- `completedActivitiesCount` or `doneActivities` → "Done activities"
- `pendingActivitiesCount` or `activitiesToDo` → "Activities to do"
- `productsCount` or `numberOfProducts` → "Number of products"
- `ownerName` or `personOwnerName` → "Owner name"

## Notes

- The API automatically handles null values (sorts them to the end for ascending, beginning for descending)
- The `nextActivity` sort finds the earliest pending activity for each deal
- Activity counts are calculated in real-time from the activities table
- The `productsCount` field currently always returns 0 (reserved for future use)
- All sorting is done server-side, so you don't need to sort the data on the frontend

