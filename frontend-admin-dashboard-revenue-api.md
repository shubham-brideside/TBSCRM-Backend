## Admin Dashboard - Revenue Calculation API (Frontend Guide)

### Overview

This document describes the backend endpoint that provides **total revenue calculation** for the Admin dashboard.  
The endpoint calculates the sum of all deal values based on the selected filters, optimized for performance by calculating on the backend instead of fetching all deals.

Use this to replace the current frontend implementation that fetches all WON deals and calculates the sum on the client side.

---

### Base URL & Authentication

- **Base URL:** `/api/deals`
- **Authentication:** JWT required, include header:

```http
Authorization: Bearer <your-jwt-token>
```

- **Roles:**
  - Endpoint is accessible to all authenticated users (respects role-based deal access).

---

### Endpoint: Calculate Revenue

- **Method:** `GET`
- **URL:** `/api/deals/revenue`
- **Auth:** Authenticated users
- **Query Parameters:** All parameters are optional and match the filters from `/api/deals` endpoint

#### Query Parameters

| Name          | Type    | Required | Description                                                      |
|---------------|---------|----------|------------------------------------------------------------------|
| pipelineId    | integer | No       | Filter by pipeline ID                                            |
| status        | string  | No       | Filter by status: `WON`, `LOST`, `IN_PROGRESS`, or `all`       |
| organizationId| integer | No       | Filter by organization ID                                         |
| categoryId    | integer | No       | Filter by category ID                                             |
| managerId     | integer | No       | Filter by manager ID (person owner)                              |
| dateFrom      | string  | No       | Start date filter (YYYY-MM-DD format)                            |
| dateTo        | string  | No       | End date filter (YYYY-MM-DD format)                               |
| search        | string  | No       | Search term (searches in deal name, venue, phone, person name, person instagram, organization name) |
| source        | string  | No       | Filter by deal source: `Direct`, `Divert`, `Reference`, `Planner`, `TBS` |
| stageId       | integer | No       | Filter by stage ID                                                |

#### Filter Behavior

- All filters work the same way as the `/api/deals` endpoint
- Filters are combined with AND logic (all must match)
- Only non-deleted deals are included (`isDeleted = false`)
- Date filters use `createdAt` field (inclusive range)
- Search is case-insensitive and searches across multiple fields

#### Response Shape

Standard wrapped `ApiResponse`:

```json
{
  "success": true,
  "message": "Revenue calculated",
  "data": {
    "totalRevenue": 1250000.50,
    "dealCount": 45,
    "currency": "INR"
  }
}
```

- **`totalRevenue`**: Sum of all deal values matching the filters (BigDecimal, defaults to 0.00 if no deals match)
- **`dealCount`**: Number of deals matching the filters (Long, defaults to 0 if no deals match)
- **`currency`**: Currency code (defaults to "INR")

---

### Example Requests

#### 1. Calculate Total Revenue for All WON Deals

```http
GET /api/deals/revenue?status=WON
Authorization: Bearer <your-jwt-token>
```

**Response:**
```json
{
  "success": true,
  "message": "Revenue calculated",
  "data": {
    "totalRevenue": 2500000.00,
    "dealCount": 120,
    "currency": "INR"
  }
}
```

#### 2. Calculate Revenue for a Specific Pipeline

```http
GET /api/deals/revenue?pipelineId=5&status=WON
Authorization: Bearer <your-jwt-token>
```

#### 3. Calculate Revenue with Multiple Filters

```http
GET /api/deals/revenue?status=WON&categoryId=3&managerId=10&dateFrom=2025-01-01&dateTo=2025-12-31
Authorization: Bearer <your-jwt-token>
```

#### 4. Calculate Revenue for All Deals (No Status Filter)

```http
GET /api/deals/revenue
Authorization: Bearer <your-jwt-token>
```

---

### Frontend Implementation Example

#### JavaScript/TypeScript (Fetch API)

```javascript
/**
 * Calculate revenue based on current dashboard filters
 * @param {Object} filters - Filter object matching the query parameters
 * @returns {Promise<{totalRevenue: number, dealCount: number}>}
 */
async function calculateRevenue(filters = {}) {
  const baseUrl = process.env.REACT_APP_API_BASE_URL || 'http://localhost:8080';
  const token = localStorage.getItem('authToken'); // Adjust based on your auth storage
  
  // Build query string from filters
  const queryParams = new URLSearchParams();
  
  if (filters.pipelineId) queryParams.append('pipelineId', filters.pipelineId);
  if (filters.status) queryParams.append('status', filters.status);
  if (filters.organizationId) queryParams.append('organizationId', filters.organizationId);
  if (filters.categoryId) queryParams.append('categoryId', filters.categoryId);
  if (filters.managerId) queryParams.append('managerId', filters.managerId);
  if (filters.dateFrom) queryParams.append('dateFrom', filters.dateFrom);
  if (filters.dateTo) queryParams.append('dateTo', filters.dateTo);
  if (filters.search) queryParams.append('search', filters.search);
  if (filters.source) queryParams.append('source', filters.source);
  if (filters.stageId) queryParams.append('stageId', filters.stageId);
  
  const url = `${baseUrl}/api/deals/revenue?${queryParams.toString()}`;
  
  try {
    const response = await fetch(url, {
      method: 'GET',
      headers: {
        'Authorization': `Bearer ${token}`,
        'Content-Type': 'application/json'
      }
    });
    
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    
    const result = await response.json();
    
    if (result.success && result.data) {
      return {
        totalRevenue: parseFloat(result.data.totalRevenue) || 0,
        dealCount: parseInt(result.data.dealCount) || 0,
        currency: result.data.currency || 'INR'
      };
    }
    
    throw new Error('Invalid response format');
  } catch (error) {
    console.error('Error calculating revenue:', error);
    throw error;
  }
}

// Usage in React component
function AdminDashboard() {
  const [revenue, setRevenue] = useState({ totalRevenue: 0, dealCount: 0 });
  const [loading, setLoading] = useState(false);
  const [filters, setFilters] = useState({
    status: 'WON' // Default to WON deals for revenue
  });
  
  useEffect(() => {
    async function loadRevenue() {
      setLoading(true);
      try {
        const data = await calculateRevenue(filters);
        setRevenue(data);
      } catch (error) {
        console.error('Failed to load revenue:', error);
        // Handle error (show toast, etc.)
      } finally {
        setLoading(false);
      }
    }
    
    loadRevenue();
  }, [filters]); // Recalculate when filters change
  
  return (
    <div>
      <h2>Revenue Till Date</h2>
      {loading ? (
        <p>Calculating...</p>
      ) : (
        <div>
          <p>Total Revenue: ₹{revenue.totalRevenue.toLocaleString('en-IN', { 
            minimumFractionDigits: 2, 
            maximumFractionDigits: 2 
          })}</p>
          <p>Deal Count: {revenue.dealCount}</p>
        </div>
      )}
      
      {/* Filter controls that update `filters` state */}
      <FilterControls 
        filters={filters} 
        onFiltersChange={setFilters} 
      />
    </div>
  );
}
```

#### React Hook Example

```typescript
import { useState, useEffect } from 'react';

interface RevenueFilters {
  pipelineId?: number;
  status?: string;
  organizationId?: number;
  categoryId?: number;
  managerId?: number;
  dateFrom?: string;
  dateTo?: string;
  search?: string;
  source?: string;
  stageId?: number;
}

interface RevenueData {
  totalRevenue: number;
  dealCount: number;
  currency: string;
}

function useRevenue(filters: RevenueFilters) {
  const [data, setData] = useState<RevenueData>({ totalRevenue: 0, dealCount: 0, currency: 'INR' });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<Error | null>(null);
  
  useEffect(() => {
    async function fetchRevenue() {
      setLoading(true);
      setError(null);
      
      try {
        const queryParams = new URLSearchParams();
        Object.entries(filters).forEach(([key, value]) => {
          if (value !== undefined && value !== null && value !== '') {
            queryParams.append(key, String(value));
          }
        });
        
        const response = await fetch(
          `${API_BASE_URL}/api/deals/revenue?${queryParams.toString()}`,
          {
            headers: {
              'Authorization': `Bearer ${getAuthToken()}`,
              'Content-Type': 'application/json'
            }
          }
        );
        
        if (!response.ok) {
          throw new Error(`Failed to calculate revenue: ${response.statusText}`);
        }
        
        const result = await response.json();
        
        if (result.success && result.data) {
          setData({
            totalRevenue: parseFloat(result.data.totalRevenue) || 0,
            dealCount: parseInt(result.data.dealCount) || 0,
            currency: result.data.currency || 'INR'
          });
        } else {
          throw new Error('Invalid response format');
        }
      } catch (err) {
        setError(err instanceof Error ? err : new Error('Unknown error'));
        setData({ totalRevenue: 0, dealCount: 0, currency: 'INR' });
      } finally {
        setLoading(false);
      }
    }
    
    fetchRevenue();
  }, [filters]);
  
  return { data, loading, error };
}

// Usage
function RevenueCard({ filters }: { filters: RevenueFilters }) {
  const { data, loading, error } = useRevenue(filters);
  
  if (loading) return <div>Calculating revenue...</div>;
  if (error) return <div>Error: {error.message}</div>;
  
  return (
    <div className="revenue-card">
      <h3>Revenue Till Date</h3>
      <p className="revenue-amount">
        ₹{data.totalRevenue.toLocaleString('en-IN', { 
          minimumFractionDigits: 2, 
          maximumFractionDigits: 2 
        })}
      </p>
      <p className="deal-count">{data.dealCount} deals</p>
    </div>
  );
}
```

---

### Migration from Current Implementation

#### Before (Inefficient - Fetching All Deals)

```javascript
// ❌ OLD WAY: Fetching all WON deals and calculating on frontend
async function getRevenue() {
  const response = await fetch('/api/deals?status=WON&limit=10000');
  const result = await response.json();
  const deals = result.data.deals || [];
  
  const totalRevenue = deals.reduce((sum, deal) => {
    return sum + (parseFloat(deal.value) || 0);
  }, 0);
  
  return totalRevenue;
}
```

#### After (Optimized - Backend Calculation)

```javascript
// ✅ NEW WAY: Backend calculates sum efficiently
async function getRevenue(filters = {}) {
  const queryParams = new URLSearchParams({ status: 'WON', ...filters });
  const response = await fetch(`/api/deals/revenue?${queryParams.toString()}`);
  const result = await response.json();
  
  return result.data.totalRevenue;
}
```

**Benefits:**
- ✅ Much faster (single SQL SUM query vs fetching thousands of deals)
- ✅ Lower network bandwidth (returns only 3 numbers vs full deal objects)
- ✅ Better scalability (works efficiently even with millions of deals)
- ✅ Consistent with other aggregation endpoints (`/api/deals/stage-totals`)

---

### Error Handling

The endpoint returns standard HTTP status codes:

- **200 OK**: Success
- **400 Bad Request**: Invalid parameters (e.g., negative IDs, invalid date format)
- **401 Unauthorized**: Missing or invalid JWT token
- **403 Forbidden**: User doesn't have access (based on role-based deal filtering)

**Example Error Response:**
```json
{
  "success": false,
  "message": "Invalid source value: InvalidSource. Allowed values: Direct, Divert, Reference, Planner, TBS",
  "data": null
}
```

---

### Performance Considerations

1. **Caching**: Consider caching revenue calculations on the frontend if filters don't change frequently
2. **Debouncing**: If filters are updated via user input, debounce API calls to avoid excessive requests
3. **Loading States**: Show loading indicators while calculating revenue
4. **Error Recovery**: Implement retry logic for failed requests

---

### Notes

- The endpoint respects the same role-based access control as `/api/deals` - users will only see revenue for deals they have access to
- Revenue is calculated using the `value` field of deals (BigDecimal)
- Null or missing deal values are treated as 0 in the sum
- The endpoint uses the same filter logic as the deals list endpoint for consistency

