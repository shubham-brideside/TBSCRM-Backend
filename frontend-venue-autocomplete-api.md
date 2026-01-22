# Venue Autocomplete API (Frontend)

This document covers the venue autocomplete integration using OLA Maps API for searching and selecting venues when creating or updating deals and persons.

---

## Overview

The venue autocomplete feature allows users to search for venues using OLA Maps API. The selected venue can be used when creating or updating deals and persons. The venue field is a simple string field - no database schema changes are required.

**Base URL:** `/api/venues`

---

## Search Venues (Autocomplete)

- **Method / URL:** `GET /api/venues/autocomplete`
- **Description:** Returns a list of venue suggestions based on the input query. Uses OLA Maps API for real-time place suggestions.
- **Authentication:** Public endpoint (no authentication required)

### Query Parameters

| Parameter | Type | Required | Description | Example |
|-----------|------|----------|-------------|---------|
| `input` | string | Yes | Search query text (venue name, address, etc.) | `"Taj Hotel"` |
| `latitude` | number | No | Latitude for location-based search | `19.0760` |
| `longitude` | number | No | Longitude for location-based search | `72.8777` |

### Request Examples

```javascript
// Basic search
GET /api/venues/autocomplete?input=Taj Hotel

// Location-based search (shows venues near a specific location)
GET /api/venues/autocomplete?input=Wedding&latitude=19.0760&longitude=72.8777

// URL encoded example
GET /api/venues/autocomplete?input=Grand%20Palace%20Mumbai
```

### Response (200 OK)

Array of `VenueAutocompleteDTO` objects:

```json
[
  {
    "description": "Taj Palace Hotel, Mumbai, Maharashtra, India",
    "placeId": "ChIJK1234567890",
    "mainText": "Taj Palace Hotel",
    "secondaryText": "Mumbai, Maharashtra, India",
    "distanceMeters": 1250
  },
  {
    "description": "Taj Lands End, Mumbai, Maharashtra, India",
    "placeId": "ChIJK0987654321",
    "mainText": "Taj Lands End",
    "secondaryText": "Mumbai, Maharashtra, India",
    "distanceMeters": 2100
  }
]
```

### Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `description` | string | Full description/address of the venue (use this for display and saving) |
| `placeId` | string | Unique place ID from OLA Maps (can be used for place details if needed) |
| `mainText` | string | Main venue name (for display) |
| `secondaryText` | string | Secondary text (usually address/location) |
| `distanceMeters` | number | Distance in meters from the provided location (only present if latitude/longitude provided) |

### Error Responses

- **400 Bad Request:** Empty or missing `input` parameter
  ```json
  {
    "message": "Bad Request"
  }
  ```

- **500 Internal Server Error:** OLA Maps API error or configuration issue
  ```json
  {
    "message": "Failed to fetch autocomplete suggestions: ..."
  }
  ```

---

## Integration with Deal Creation

### Using Venue in Deal Creation

When creating a deal, include the `venue` field with the selected venue description:

```javascript
// 1. User searches for venues
const searchVenues = async (query) => {
  const response = await fetch(
    `/api/venues/autocomplete?input=${encodeURIComponent(query)}`
  );
  if (!response.ok) throw new Error('Failed to search venues');
  return await response.json();
};

// 2. User selects a venue from suggestions
const selectedVenue = {
  description: "Taj Palace Hotel, Mumbai, Maharashtra, India",
  placeId: "ChIJK1234567890",
  mainText: "Taj Palace Hotel",
  secondaryText: "Mumbai, Maharashtra, India"
};

// 3. Create deal with selected venue
const createDeal = async (dealData) => {
  const response = await fetch('/api/deals', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`
    },
    body: JSON.stringify({
      name: "Wedding Package - Sharma",
      value: 125000,
      personId: 101,
      pipelineId: 8,
      stageId: 37,
      venue: selectedVenue.description, // ← Use description from autocomplete
      // ... other deal fields
    })
  });
  return await response.json();
};
```

### Deal Creation Request

```json
POST /api/deals
{
  "name": "Wedding Package - Sharma",
  "value": 125000,
  "personId": 101,
  "pipelineId": 8,
  "stageId": 37,
  "venue": "Taj Palace Hotel, Mumbai, Maharashtra, India",
  // ... other fields
}
```

### Deal Response

The created deal will include the venue:

```json
{
  "id": 215,
  "name": "Wedding Package - Sharma",
  "venue": "Taj Palace Hotel, Mumbai, Maharashtra, India",
  // ... other fields
}
```

---

## Integration with Person Creation

### Using Venue in Person Creation

When creating a person, include the `venue` field:

```javascript
// Create person with venue
const createPerson = async (personData) => {
  const response = await fetch('/api/persons', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`
    },
    body: JSON.stringify({
      name: "Aditi Sharma",
      phone: "+91 98765 43210",
      email: "aditi@example.com",
      venue: selectedVenue.description, // ← Use description from autocomplete
      // ... other person fields
    })
  });
  return await response.json();
};
```

---

## Frontend Implementation Guide

### React Component Example

```jsx
import React, { useState, useEffect, useCallback } from 'react';
import { debounce } from 'lodash'; // or implement your own debounce

function VenueAutocomplete({ onSelect, initialValue = '' }) {
  const [query, setQuery] = useState(initialValue);
  const [suggestions, setSuggestions] = useState([]);
  const [loading, setLoading] = useState(false);
  const [showSuggestions, setShowSuggestions] = useState(false);
  const [selectedVenue, setSelectedVenue] = useState(null);

  // Debounce search to avoid too many API calls
  const debouncedSearch = useCallback(
    debounce(async (searchQuery) => {
      if (!searchQuery || searchQuery.trim().length < 2) {
        setSuggestions([]);
        setLoading(false);
        return;
      }

      setLoading(true);
      try {
        const response = await fetch(
          `/api/venues/autocomplete?input=${encodeURIComponent(searchQuery)}`
        );
        
        if (!response.ok) {
          throw new Error('Failed to fetch venues');
        }
        
        const data = await response.json();
        setSuggestions(data);
        setShowSuggestions(true);
      } catch (error) {
        console.error('Error searching venues:', error);
        setSuggestions([]);
      } finally {
        setLoading(false);
      }
    }, 300), // 300ms debounce
    []
  );

  useEffect(() => {
    debouncedSearch(query);
  }, [query, debouncedSearch]);

  const handleSelect = (venue) => {
    setSelectedVenue(venue);
    setQuery(venue.description);
    setShowSuggestions(false);
    onSelect(venue);
  };

  const handleInputChange = (e) => {
    setQuery(e.target.value);
    setSelectedVenue(null);
  };

  return (
    <div className="venue-autocomplete">
      <label htmlFor="venue">Venue</label>
      <div className="autocomplete-wrapper">
        <input
          id="venue"
          type="text"
          value={query}
          onChange={handleInputChange}
          onFocus={() => {
            if (suggestions.length > 0) {
              setShowSuggestions(true);
            }
          }}
          placeholder="Search for a venue..."
          autoComplete="off"
        />
        {loading && <span className="loading">Searching...</span>}
        
        {showSuggestions && suggestions.length > 0 && (
          <ul className="suggestions-list">
            {suggestions.map((venue, index) => (
              <li
                key={venue.placeId || index}
                onClick={() => handleSelect(venue)}
                className="suggestion-item"
              >
                <div className="suggestion-main">{venue.mainText}</div>
                {venue.secondaryText && (
                  <div className="suggestion-secondary">{venue.secondaryText}</div>
                )}
                {venue.distanceMeters && (
                  <div className="suggestion-distance">
                    {(venue.distanceMeters / 1000).toFixed(1)} km away
                  </div>
                )}
              </li>
            ))}
          </ul>
        )}
        
        {showSuggestions && !loading && query.length >= 2 && suggestions.length === 0 && (
          <div className="no-results">No venues found</div>
        )}
      </div>
    </div>
  );
}

export default VenueAutocomplete;
```

### Usage in Deal Form

```jsx
function CreateDealForm() {
  const [dealData, setDealData] = useState({
    name: '',
    value: '',
    venue: '', // Will be set from autocomplete
    // ... other fields
  });

  const handleVenueSelect = (venue) => {
    setDealData(prev => ({
      ...prev,
      venue: venue.description // Save the full description
    }));
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    
    const response = await fetch('/api/deals', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
      },
      body: JSON.stringify(dealData)
    });
    
    // Handle response...
  };

  return (
    <form onSubmit={handleSubmit}>
      <input
        name="name"
        value={dealData.name}
        onChange={(e) => setDealData({ ...dealData, name: e.target.value })}
        placeholder="Deal Name"
      />
      
      <VenueAutocomplete
        onSelect={handleVenueSelect}
        initialValue={dealData.venue}
      />
      
      {/* Other form fields */}
      
      <button type="submit">Create Deal</button>
    </form>
  );
}
```

### Location-Based Search (Optional)

If you have the user's location, you can provide latitude and longitude for more relevant results:

```javascript
// Get user's location (with permission)
navigator.geolocation.getCurrentPosition(
  (position) => {
    const { latitude, longitude } = position.coords;
    
    // Use location in search
    const response = await fetch(
      `/api/venues/autocomplete?input=${encodeURIComponent(query)}&latitude=${latitude}&longitude=${longitude}`
    );
    // ... handle response
  },
  (error) => {
    console.error('Error getting location:', error);
    // Fallback to search without location
  }
);
```

---

## Best Practices

### 1. Debouncing
Always debounce the search input to avoid making too many API calls:
- Wait at least 300ms after user stops typing
- Don't search if query is less than 2-3 characters

### 2. Loading States
Show loading indicators while searching:
```jsx
{loading && <span>Searching venues...</span>}
```

### 3. Error Handling
Handle API errors gracefully:
```javascript
try {
  const response = await fetch(...);
  if (!response.ok) {
    throw new Error('Failed to search venues');
  }
  // ... handle success
} catch (error) {
  console.error('Venue search error:', error);
  // Show user-friendly error message
}
```

### 4. Empty States
Show helpful messages when no results are found:
```jsx
{suggestions.length === 0 && query.length >= 2 && !loading && (
  <div>No venues found. Try a different search term.</div>
)}
```

### 5. Keyboard Navigation
Implement keyboard navigation for better UX:
- Arrow keys to navigate suggestions
- Enter to select
- Escape to close

### 6. Storing Selected Venue
Always store the `description` field (full address) in the database:
- It's the most complete venue information
- It's what the backend expects
- It can be displayed to users later

---

## CSS Styling Example

```css
.venue-autocomplete {
  position: relative;
  margin-bottom: 1rem;
}

.autocomplete-wrapper {
  position: relative;
}

.autocomplete-wrapper input {
  width: 100%;
  padding: 0.5rem;
  border: 1px solid #ccc;
  border-radius: 4px;
  font-size: 1rem;
}

.autocomplete-wrapper input:focus {
  outline: none;
  border-color: #007bff;
}

.suggestions-list {
  position: absolute;
  top: 100%;
  left: 0;
  right: 0;
  background: white;
  border: 1px solid #ccc;
  border-top: none;
  border-radius: 0 0 4px 4px;
  max-height: 300px;
  overflow-y: auto;
  z-index: 1000;
  margin: 0;
  padding: 0;
  list-style: none;
}

.suggestion-item {
  padding: 0.75rem;
  cursor: pointer;
  border-bottom: 1px solid #eee;
}

.suggestion-item:hover {
  background-color: #f5f5f5;
}

.suggestion-item:last-child {
  border-bottom: none;
}

.suggestion-main {
  font-weight: 500;
  color: #333;
}

.suggestion-secondary {
  font-size: 0.875rem;
  color: #666;
  margin-top: 0.25rem;
}

.suggestion-distance {
  font-size: 0.75rem;
  color: #999;
  margin-top: 0.25rem;
}

.loading {
  position: absolute;
  right: 0.5rem;
  top: 50%;
  transform: translateY(-50%);
  color: #666;
  font-size: 0.875rem;
}

.no-results {
  padding: 0.75rem;
  color: #666;
  text-align: center;
  font-style: italic;
}
```

---

## Testing

### Test Cases

1. **Basic Search**
   - Type "Taj" → Should show Taj-related venues
   - Select a venue → Should populate the input field

2. **Empty Results**
   - Type "xyz123nonexistent" → Should show "No venues found"

3. **Loading State**
   - Type quickly → Should show loading indicator
   - Should debounce requests

4. **Error Handling**
   - Simulate API error → Should show error message gracefully

5. **Location-Based Search**
   - Provide lat/lng → Results should include distance

6. **Integration**
   - Select venue → Create deal → Verify venue is saved correctly

---

## API Endpoints Summary

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/venues/autocomplete` | GET | Search venues using OLA Maps autocomplete |
| `/api/deals` | POST | Create deal (includes `venue` field) |
| `/api/deals/{id}` | PATCH | Update deal (includes `venue` field) |
| `/api/persons` | POST | Create person (includes `venue` field) |
| `/api/persons/{id}` | PATCH | Update person (includes `venue` field) |

---

## Notes

- The venue field is a **string field** - no special formatting required
- Store the full `description` from the autocomplete response
- The `placeId` can be used for future enhancements (e.g., getting place details)
- The endpoint is **public** (no authentication required) for easier frontend integration
- Location-based search is optional but recommended for better results
- Always debounce search requests to avoid rate limiting

---

For questions or issues, refer to the backend API documentation or contact the backend team.


