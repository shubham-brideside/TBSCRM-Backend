# Page Access Management - Frontend Integration Guide

## Overview

This document outlines the frontend changes required to implement a page access management system where administrators can control which pages each user can access. The system allows granular control over page visibility on a per-user basis.

## Table of Contents

1. [API Endpoints](#api-endpoints)
2. [Login Flow Integration](#login-flow-integration)
3. [Page Access State Management](#page-access-state-management)
4. [Route Protection](#route-protection)
5. [Admin Page Access Management UI](#admin-page-access-management-ui)
6. [Navigation Menu Filtering](#navigation-menu-filtering)
7. [Implementation Examples](#implementation-examples)
8. [Testing Checklist](#testing-checklist)

---

## API Endpoints

### 1. Get Current User Page Access

**Endpoint:** `GET /api/auth/page-access`

**Description:** Retrieve page access permissions for the currently authenticated user. This should be called after login.

**Authentication:** Required (Bearer token)

**Response:**
```json
{
  "success": true,
  "message": "Page access retrieved successfully",
  "data": {
    "userId": 123,
    "pageAccess": {
      "persons": true,
      "deals": true,
      "calendar": true,
      "teams": false,
      "activities": true,
      "organizations": false,
      "users": false,
      "targets": true,
      "pipelines": true,
      "dashboard_sales": false,
      "dashboard_category_manager": false,
      "dashboard_pre_sales": false,
      "reports_deal_source": true,
      "reports_deal_sub_source": true,
      "reports_deal_status": true,
      "reports_deal_lost_reason": true,
      "reports_deal_duration": true,
      "page_access_management": true
    }
  }
}
```

---

### 2. Get User Page Access (Admin Only)

**Endpoint:** `GET /api/users/{userId}/page-access`

**Description:** Retrieve all page access permissions for a specific user. Only accessible by admin or the user themselves.

**Authentication:** Required (Bearer token)

**Response:** Same structure as above

---

### 3. Update User Page Access (Bulk) - Admin Only

**Endpoint:** `PUT /api/users/{userId}/page-access`

**Description:** Update page access permissions for a specific user. This is a bulk update operation.

**Authentication:** Required (Admin only)

**Request:**
```json
{
  "pageAccess": {
    "persons": true,
    "deals": true,
    "calendar": true,
    "teams": false,
    "activities": true,
    "organizations": false,
    "users": false,
    "targets": true,
    "pipelines": true,
    "dashboard_sales": false,
    "dashboard_category_manager": false,
    "dashboard_pre_sales": false,
    "reports_deal_source": true,
    "reports_deal_sub_source": true,
    "reports_deal_status": true,
    "reports_deal_lost_reason": true,
    "reports_deal_duration": true
  }
}
```

**Response:** Same structure as GET response

---

### 4. Update Single Page Access - Admin Only

**Endpoint:** `PATCH /api/users/{userId}/page-access/{pageName}`

**Description:** Update access for a single page for a specific user.

**Authentication:** Required (Admin only)

**Request:**
```json
{
  "hasAccess": true
}
```

**Response:**
```json
{
  "success": true,
  "message": "Page access updated successfully",
  "data": {
    "userId": 123,
    "pageName": "persons",
    "hasAccess": true
  }
}
```

---

### 5. Get All Users with Page Access Summary - Admin Only

**Endpoint:** `GET /api/users/page-access-summary`

**Description:** Get a summary of page access for all users. Useful for the admin page access management interface.

**Authentication:** Required (Admin only)

**Response:**
```json
{
  "success": true,
  "message": "Page access summary retrieved successfully",
  "data": [
    {
      "userId": 1,
      "email": "user1@example.com",
      "firstName": "John",
      "lastName": "Doe",
      "role": "SALES",
      "pageAccess": {
        "persons": true,
        "deals": true,
        "calendar": true,
        "teams": false,
        "activities": true,
        "organizations": false,
        "users": false,
        "targets": true,
        "pipelines": true,
        "dashboard_sales": true,
        "dashboard_category_manager": false,
        "dashboard_pre_sales": false,
        "reports_deal_source": true,
        "reports_deal_sub_source": true,
        "reports_deal_status": true,
        "reports_deal_lost_reason": true,
        "reports_deal_duration": true
      }
    },
    {
      "userId": 2,
      "email": "user2@example.com",
      "firstName": "Jane",
      "lastName": "Smith",
      "role": "ADMIN",
      "pageAccess": {
        "persons": true,
        "deals": true,
        "calendar": true,
        "teams": true,
        "activities": true,
        "organizations": true,
        "users": true,
        "targets": true,
        "pipelines": true,
        "dashboard_sales": false,
        "dashboard_category_manager": false,
        "dashboard_pre_sales": false,
        "reports_deal_source": true,
        "reports_deal_sub_source": true,
        "reports_deal_sub_source": true,
        "reports_deal_status": true,
        "reports_deal_lost_reason": true,
        "reports_deal_duration": true,
        "page_access_management": true
      }
    }
  ]
}
```

---

## Login Flow Integration

### Step 1: Update Login Response Handling

After successful login, you should:

1. Store the JWT token
2. **Call the page access endpoint** to get user's page permissions
3. Store page access in your state management (Redux, Context, etc.)
4. Redirect to dashboard

**Example (React with Context API):**

```typescript
// AuthContext.tsx or similar
const login = async (email: string, password: string) => {
  try {
    // 1. Login
    const loginResponse = await api.post('/api/auth/login', { email, password });
    const { token, userId, role } = loginResponse.data.data;
    
    // Store token
    localStorage.setItem('token', token);
    setAuthToken(token);
    
    // 2. Get page access
    const pageAccessResponse = await api.get('/api/auth/page-access', {
      headers: { Authorization: `Bearer ${token}` }
    });
    const pageAccess = pageAccessResponse.data.data.pageAccess;
    
    // 3. Store in state
    setUser({ userId, role, pageAccess });
    setPageAccess(pageAccess);
    
    // 4. Redirect
    navigate('/dashboard');
  } catch (error) {
    // Handle error
  }
};
```

### Step 2: Refresh Page Access (Optional)

You may want to refresh page access when:
- User navigates to a protected route
- After admin updates page access
- On app initialization (if token exists)

```typescript
const refreshPageAccess = async () => {
  try {
    const response = await api.get('/api/auth/page-access');
    const pageAccess = response.data.data.pageAccess;
    setPageAccess(pageAccess);
    return pageAccess;
  } catch (error) {
    // Handle error - maybe token expired
    logout();
  }
};
```

---

## Page Access State Management

### Option 1: React Context API

```typescript
// PageAccessContext.tsx
import React, { createContext, useContext, useState, useEffect } from 'react';

interface PageAccessContextType {
  pageAccess: Record<string, boolean> | null;
  hasAccess: (pageName: string) => boolean;
  refreshPageAccess: () => Promise<void>;
}

const PageAccessContext = createContext<PageAccessContextType | undefined>(undefined);

export const PageAccessProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const [pageAccess, setPageAccess] = useState<Record<string, boolean> | null>(null);

  const hasAccess = (pageName: string): boolean => {
    if (!pageAccess) return false;
    return pageAccess[pageName] === true;
  };

  const refreshPageAccess = async () => {
    try {
      const response = await api.get('/api/auth/page-access');
      setPageAccess(response.data.data.pageAccess);
    } catch (error) {
      console.error('Failed to refresh page access:', error);
    }
  };

  return (
    <PageAccessContext.Provider value={{ pageAccess, hasAccess, refreshPageAccess }}>
      {children}
    </PageAccessContext.Provider>
  );
};

export const usePageAccess = () => {
  const context = useContext(PageAccessContext);
  if (!context) {
    throw new Error('usePageAccess must be used within PageAccessProvider');
  }
  return context;
};
```

### Option 2: Redux/Redux Toolkit

```typescript
// pageAccessSlice.ts
import { createSlice, createAsyncThunk } from '@reduxjs/toolkit';

export const fetchPageAccess = createAsyncThunk(
  'pageAccess/fetch',
  async (_, { getState }) => {
    const response = await api.get('/api/auth/page-access');
    return response.data.data.pageAccess;
  }
);

const pageAccessSlice = createSlice({
  name: 'pageAccess',
  initialState: {
    pageAccess: null as Record<string, boolean> | null,
    loading: false,
    error: null,
  },
  reducers: {
    clearPageAccess: (state) => {
      state.pageAccess = null;
    },
  },
  extraReducers: (builder) => {
    builder
      .addCase(fetchPageAccess.pending, (state) => {
        state.loading = true;
      })
      .addCase(fetchPageAccess.fulfilled, (state, action) => {
        state.loading = false;
        state.pageAccess = action.payload;
      })
      .addCase(fetchPageAccess.rejected, (state, action) => {
        state.loading = false;
        state.error = action.error.message;
      });
  },
});

export const { clearPageAccess } = pageAccessSlice.actions;
export default pageAccessSlice.reducer;

// Selector
export const selectHasPageAccess = (state: RootState, pageName: string): boolean => {
  return state.pageAccess.pageAccess?.[pageName] === true;
};
```

---

## Route Protection

### React Router Protection

Create a protected route component that checks page access:

```typescript
// ProtectedRoute.tsx
import { Navigate } from 'react-router-dom';
import { usePageAccess } from './PageAccessContext';

interface ProtectedRouteProps {
  children: React.ReactNode;
  requiredPage: string; // e.g., 'persons', 'deals', etc.
  fallbackPath?: string;
}

export const ProtectedRoute: React.FC<ProtectedRouteProps> = ({
  children,
  requiredPage,
  fallbackPath = '/dashboard',
}) => {
  const { hasAccess } = usePageAccess();

  if (!hasAccess(requiredPage)) {
    // Redirect to dashboard or show access denied page
    return <Navigate to={fallbackPath} replace />;
  }

  return <>{children}</>;
};
```

### Usage in Routes

```typescript
// App.tsx or Router.tsx
import { ProtectedRoute } from './components/ProtectedRoute';

<Routes>
  <Route path="/login" element={<Login />} />
  <Route path="/dashboard" element={<Dashboard />} />
  
  <Route
    path="/persons"
    element={
      <ProtectedRoute requiredPage="persons">
        <PersonsPage />
      </ProtectedRoute>
    }
  />
  
  <Route
    path="/deals"
    element={
      <ProtectedRoute requiredPage="deals">
        <DealsPage />
      </ProtectedRoute>
    }
  />
  
  <Route
    path="/calendar"
    element={
      <ProtectedRoute requiredPage="calendar">
        <CalendarPage />
      </ProtectedRoute>
    }
  />
  
  {/* Admin only route */}
  <Route
    path="/admin/page-access"
    element={
      <ProtectedRoute requiredPage="page_access_management">
        <PageAccessManagementPage />
      </ProtectedRoute>
    }
  />
</Routes>
```

### Alternative: Route Guard Hook

```typescript
// useRouteGuard.ts
import { useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { usePageAccess } from './PageAccessContext';

export const useRouteGuard = (requiredPage: string) => {
  const { hasAccess, pageAccess } = usePageAccess();
  const navigate = useNavigate();

  useEffect(() => {
    // Wait for page access to load
    if (pageAccess === null) return;

    if (!hasAccess(requiredPage)) {
      navigate('/dashboard', { replace: true });
    }
  }, [pageAccess, requiredPage, hasAccess, navigate]);

  return hasAccess(requiredPage);
};

// Usage in component
const PersonsPage = () => {
  useRouteGuard('persons');
  // ... rest of component
};
```

---

## Navigation Menu Filtering

Filter navigation items based on page access:

```typescript
// Navigation.tsx or Sidebar.tsx
import { usePageAccess } from './PageAccessContext';

const Navigation = () => {
  const { hasAccess } = usePageAccess();

  const menuItems = [
    { path: '/dashboard', label: 'Dashboard', icon: DashboardIcon },
    { path: '/persons', label: 'Persons', icon: PersonIcon, page: 'persons' },
    { path: '/deals', label: 'Deals', icon: DealIcon, page: 'deals' },
    { path: '/calendar', label: 'Calendar', icon: CalendarIcon, page: 'calendar' },
    { path: '/teams', label: 'Teams', icon: TeamIcon, page: 'teams' },
    { path: '/activities', label: 'Activities', icon: ActivityIcon, page: 'activities' },
    { path: '/organizations', label: 'Organizations', icon: OrgIcon, page: 'organizations' },
    { path: '/users', label: 'Users', icon: UserIcon, page: 'users' },
    { path: '/targets', label: 'Targets', icon: TargetIcon, page: 'targets' },
    { path: '/pipelines', label: 'Pipelines', icon: PipelineIcon, page: 'pipelines' },
    { path: '/admin/page-access', label: 'Page Access', icon: SettingsIcon, page: 'page_access_management' },
  ];

  // Filter menu items based on page access
  const visibleItems = menuItems.filter(item => {
    // Dashboard is always visible
    if (!item.page) return true;
    // Check if user has access to this page
    return hasAccess(item.page);
  });

  return (
    <nav>
      {visibleItems.map(item => (
        <NavLink key={item.path} to={item.path}>
          <item.icon />
          {item.label}
        </NavLink>
      ))}
    </nav>
  );
};
```

---

## Admin Page Access Management UI

Create a page where admins can manage page access for all users.

### Page Structure

```typescript
// PageAccessManagementPage.tsx
import React, { useState, useEffect } from 'react';
import { usePageAccess } from '../contexts/PageAccessContext';

interface UserPageAccess {
  userId: number;
  email: string;
  firstName: string;
  lastName: string;
  role: string;
  pageAccess: Record<string, boolean>;
}

const PageAccessManagementPage = () => {
  const [users, setUsers] = useState<UserPageAccess[]>([]);
  const [loading, setLoading] = useState(true);
  const [selectedUserId, setSelectedUserId] = useState<number | null>(null);
  const [saving, setSaving] = useState(false);

  // All available pages
  const allPages = [
    { key: 'persons', label: 'Persons' },
    { key: 'deals', label: 'Deals' },
    { key: 'calendar', label: 'Calendar' },
    { key: 'teams', label: 'Teams' },
    { key: 'activities', label: 'Activities' },
    { key: 'organizations', label: 'Organizations' },
    { key: 'users', label: 'Users' },
    { key: 'targets', label: 'Targets' },
    { key: 'pipelines', label: 'Pipelines' },
    { key: 'dashboard_sales', label: 'Sales Dashboard' },
    { key: 'dashboard_category_manager', label: 'Category Manager Dashboard' },
    { key: 'dashboard_pre_sales', label: 'Pre-Sales Dashboard' },
    { key: 'reports_deal_source', label: 'Deal Source Report' },
    { key: 'reports_deal_sub_source', label: 'Deal Sub Source Report' },
    { key: 'reports_deal_status', label: 'Deal Status Report' },
    { key: 'reports_deal_lost_reason', label: 'Deal Lost Reason Report' },
    { key: 'reports_deal_duration', label: 'Deal Duration Report' },
  ];

  useEffect(() => {
    fetchUsers();
  }, []);

  const fetchUsers = async () => {
    try {
      setLoading(true);
      const response = await api.get('/api/users/page-access-summary');
      setUsers(response.data.data);
    } catch (error) {
      console.error('Failed to fetch users:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleTogglePageAccess = async (userId: number, pageName: string, hasAccess: boolean) => {
    try {
      setSaving(true);
      await api.patch(`/api/users/${userId}/page-access/${pageName}`, {
        hasAccess: !hasAccess,
      });
      // Refresh the list
      await fetchUsers();
    } catch (error) {
      console.error('Failed to update page access:', error);
      alert('Failed to update page access');
    } finally {
      setSaving(false);
    }
  };

  const handleBulkUpdate = async (userId: number, pageAccess: Record<string, boolean>) => {
    try {
      setSaving(true);
      await api.put(`/api/users/${userId}/page-access`, { pageAccess });
      await fetchUsers();
      alert('Page access updated successfully');
    } catch (error) {
      console.error('Failed to update page access:', error);
      alert('Failed to update page access');
    } finally {
      setSaving(false);
    }
  };

  if (loading) {
    return <div>Loading...</div>;
  }

  return (
    <div className="page-access-management">
      <h1>Page Access Management</h1>
      
      <div className="users-list">
        {users.map(user => (
          <div key={user.userId} className="user-card">
            <div className="user-header">
              <h3>{user.firstName} {user.lastName}</h3>
              <p>{user.email} ({user.role})</p>
            </div>
            
            <div className="page-access-grid">
              {allPages.map(page => (
                <label key={page.key} className="page-access-item">
                  <input
                    type="checkbox"
                    checked={user.pageAccess[page.key] === true}
                    onChange={() => handleTogglePageAccess(
                      user.userId,
                      page.key,
                      user.pageAccess[page.key] === true
                    )}
                    disabled={saving}
                  />
                  <span>{page.label}</span>
                </label>
              ))}
            </div>
          </div>
        ))}
      </div>
    </div>
  );
};

export default PageAccessManagementPage;
```

### Enhanced Version with Bulk Actions

```typescript
// Enhanced version with select all, save all, etc.
const PageAccessManagementPage = () => {
  // ... previous code ...

  const [editingUser, setEditingUser] = useState<number | null>(null);
  const [editedPageAccess, setEditedPageAccess] = useState<Record<string, boolean>>({});

  const startEditing = (user: UserPageAccess) => {
    setEditingUser(user.userId);
    setEditedPageAccess({ ...user.pageAccess });
  };

  const cancelEditing = () => {
    setEditingUser(null);
    setEditedPageAccess({});
  };

  const handleBulkSave = async (userId: number) => {
    await handleBulkUpdate(userId, editedPageAccess);
    cancelEditing();
  };

  const toggleAllPages = (enabled: boolean) => {
    const newAccess: Record<string, boolean> = {};
    allPages.forEach(page => {
      newAccess[page.key] = enabled;
    });
    setEditedPageAccess(newAccess);
  };

  return (
    <div className="page-access-management">
      <h1>Page Access Management</h1>
      
      {users.map(user => (
        <div key={user.userId} className="user-card">
          <div className="user-header">
            <h3>{user.firstName} {user.lastName}</h3>
            <p>{user.email} ({user.role})</p>
            {editingUser === user.userId ? (
              <div>
                <button onClick={() => toggleAllPages(true)}>Select All</button>
                <button onClick={() => toggleAllPages(false)}>Deselect All</button>
                <button onClick={() => handleBulkSave(user.userId)}>Save</button>
                <button onClick={cancelEditing}>Cancel</button>
              </div>
            ) : (
              <button onClick={() => startEditing(user)}>Edit</button>
            )}
          </div>
          
          <div className="page-access-grid">
            {allPages.map(page => {
              const isEditing = editingUser === user.userId;
              const access = isEditing 
                ? editedPageAccess[page.key] === true
                : user.pageAccess[page.key] === true;
              
              return (
                <label key={page.key} className="page-access-item">
                  <input
                    type="checkbox"
                    checked={access}
                    onChange={() => {
                      if (isEditing) {
                        setEditedPageAccess({
                          ...editedPageAccess,
                          [page.key]: !access,
                        });
                      } else {
                        handleTogglePageAccess(user.userId, page.key, access);
                      }
                    }}
                    disabled={saving && !isEditing}
                  />
                  <span>{page.label}</span>
                </label>
              );
            })}
          </div>
        </div>
      ))}
    </div>
  );
};
```

---

## Implementation Examples

### Example 1: Conditional Rendering Based on Page Access

```typescript
// SomeComponent.tsx
import { usePageAccess } from '../contexts/PageAccessContext';

const SomeComponent = () => {
  const { hasAccess } = usePageAccess();

  return (
    <div>
      {hasAccess('deals') && (
        <button onClick={() => navigate('/deals')}>
          View Deals
        </button>
      )}
      
      {hasAccess('reports_deal_source') && (
        <Link to="/reports/deal-source">Deal Source Report</Link>
      )}
    </div>
  );
};
```

### Example 2: API Service

```typescript
// api/pageAccess.ts
import api from './api';

export const pageAccessService = {
  getCurrentUserPageAccess: async () => {
    const response = await api.get('/api/auth/page-access');
    return response.data.data.pageAccess;
  },

  getUserPageAccess: async (userId: number) => {
    const response = await api.get(`/api/users/${userId}/page-access`);
    return response.data.data.pageAccess;
  },

  updateUserPageAccess: async (userId: number, pageAccess: Record<string, boolean>) => {
    const response = await api.put(`/api/users/${userId}/page-access`, { pageAccess });
    return response.data.data;
  },

  updateSinglePageAccess: async (userId: number, pageName: string, hasAccess: boolean) => {
    const response = await api.patch(`/api/users/${userId}/page-access/${pageName}`, {
      hasAccess,
    });
    return response.data.data;
  },

  getAllUsersPageAccessSummary: async () => {
    const response = await api.get('/api/users/page-access-summary');
    return response.data.data;
  },
};
```

### Example 3: TypeScript Types

```typescript
// types/pageAccess.ts
export interface PageAccess {
  userId: number;
  pageAccess: Record<string, boolean>;
}

export interface UserPageAccessSummary {
  userId: number;
  email: string;
  firstName: string;
  lastName: string;
  role: string;
  pageAccess: Record<string, boolean>;
}

export type PageName =
  | 'persons'
  | 'deals'
  | 'calendar'
  | 'teams'
  | 'activities'
  | 'organizations'
  | 'users'
  | 'targets'
  | 'pipelines'
  | 'dashboard_sales'
  | 'dashboard_category_manager'
  | 'dashboard_pre_sales'
  | 'reports_deal_source'
  | 'reports_deal_sub_source'
  | 'reports_deal_status'
  | 'reports_deal_lost_reason'
  | 'reports_deal_duration'
  | 'page_access_management';
```

---

## Testing Checklist

### Functional Testing

- [ ] Login flow correctly fetches and stores page access
- [ ] Navigation menu only shows pages user has access to
- [ ] Protected routes redirect users without access
- [ ] Admin can view page access management page
- [ ] Admin can view all users' page access summary
- [ ] Admin can update page access for users (bulk)
- [ ] Admin can update single page access for users
- [ ] Non-admin users cannot access admin endpoints
- [ ] Users can view their own page access
- [ ] Page access persists after page refresh (if stored in localStorage/sessionStorage)
- [ ] Logout clears page access state

### Edge Cases

- [ ] User with no page access records (should use role-based defaults)
- [ ] User with partial page access (some pages true, some false)
- [ ] Admin user always has access to page_access_management
- [ ] Invalid page names are rejected
- [ ] Network errors are handled gracefully
- [ ] Token expiration during page access check

### UI/UX Testing

- [ ] Loading states are shown during API calls
- [ ] Error messages are displayed appropriately
- [ ] Success messages are shown after updates
- [ ] Page access management UI is intuitive
- [ ] Bulk update works correctly
- [ ] Single page toggle works correctly
- [ ] Changes are reflected immediately after update

---

## Page Name Constants

For consistency, use these exact page names (they must match backend):

```typescript
export const PAGE_NAMES = {
  // Main pages
  PERSONS: 'persons',
  DEALS: 'deals',
  CALENDAR: 'calendar',
  TEAMS: 'teams',
  ACTIVITIES: 'activities',
  ORGANIZATIONS: 'organizations',
  USERS: 'users',
  TARGETS: 'targets',
  PIPELINES: 'pipelines',
  
  // Dashboard pages
  DASHBOARD_SALES: 'dashboard_sales',
  DASHBOARD_CATEGORY_MANAGER: 'dashboard_category_manager',
  DASHBOARD_PRE_SALES: 'dashboard_pre_sales',
  
  // Report pages
  REPORTS_DEAL_SOURCE: 'reports_deal_source',
  REPORTS_DEAL_SUB_SOURCE: 'reports_deal_sub_source',
  REPORTS_DEAL_STATUS: 'reports_deal_status',
  REPORTS_DEAL_LOST_REASON: 'reports_deal_lost_reason',
  REPORTS_DEAL_DURATION: 'reports_deal_duration',
  
  // Admin page
  PAGE_ACCESS_MANAGEMENT: 'page_access_management',
} as const;
```

---

## Best Practices

1. **Always check page access before rendering protected content**
2. **Cache page access in state management** to avoid unnecessary API calls
3. **Refresh page access after admin updates** to reflect changes immediately
4. **Handle loading and error states** gracefully
5. **Use TypeScript** for type safety with page names
6. **Validate page names** before making API calls
7. **Show appropriate error messages** when access is denied
8. **Consider implementing optimistic updates** for better UX
9. **Add unit tests** for page access logic
10. **Document page name mappings** between frontend routes and backend constants

---

## Migration Steps

1. **Update Login Flow**
   - Add page access API call after login
   - Store page access in state management

2. **Add Page Access Context/Store**
   - Create context or Redux slice for page access
   - Implement `hasAccess` helper function

3. **Update Route Protection**
   - Add `ProtectedRoute` component
   - Wrap protected routes with page access checks

4. **Update Navigation**
   - Filter menu items based on page access
   - Hide inaccessible pages from navigation

5. **Create Admin UI**
   - Build page access management page
   - Implement bulk and single update functionality

6. **Test Thoroughly**
   - Test all user roles
   - Test edge cases
   - Test admin functionality

---

## Notes

- Page access is checked on the backend as well, so even if frontend checks are bypassed, the API will return 403 Forbidden
- Admin users always have access to `page_access_management` page regardless of their page_access records
- If a user has no page_access records, role-based defaults are applied (see backend documentation)
- Page names must match exactly between frontend and backend (case-sensitive)

