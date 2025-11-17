# CORS Error Troubleshooting Guide

## The Issue
You're getting CORS errors when the frontend tries to call the backend API.

## Root Cause Analysis

### If it was working before and backend hasn't changed:

**Most likely causes (Frontend side):**

1. **Frontend started sending `withCredentials: true`**
   - Check your axios/fetch configuration
   - Look for: `axios.defaults.withCredentials = true` or `credentials: 'include'`
   - **Fix:** Remove `withCredentials` or set it to `false`

2. **Frontend changed the API base URL**
   - Check if the frontend is now calling the backend directly instead of using a proxy
   - **Fix:** Either use a proxy or ensure CORS is properly configured

3. **Frontend added custom headers**
   - Custom headers trigger CORS preflight requests
   - Check if you added any custom headers like `X-Custom-Header`
   - **Fix:** Ensure backend allows those headers in CORS config

4. **Frontend changed HTTP client library**
   - Different libraries handle CORS differently
   - **Fix:** Check the library's default CORS behavior

## Frontend Checks

### 1. Check Axios Configuration

```javascript
// ❌ BAD - This will cause CORS errors with wildcard origins
axios.defaults.withCredentials = true;

// ✅ GOOD - Don't send credentials, or use specific origins
axios.defaults.withCredentials = false;
```

### 2. Check Fetch Configuration

```javascript
// ❌ BAD - This will cause CORS errors
fetch(url, {
  credentials: 'include'  // or 'same-origin'
});

// ✅ GOOD
fetch(url, {
  credentials: 'omit'  // or remove credentials entirely
});
```

### 3. Check API Base URL

```javascript
// Check your API configuration
const API_BASE_URL = process.env.VITE_API_BASE_URL || 'http://localhost:8080';

// If using proxy in development, make sure it's configured correctly
// vite.config.js or webpack.config.js
```

### 4. Check for Custom Headers

```javascript
// ❌ Custom headers trigger preflight
axios.get(url, {
  headers: {
    'X-Custom-Header': 'value'  // This triggers preflight
  }
});

// ✅ Only use standard headers or ensure backend allows them
```

## Backend Configuration (Already Fixed)

The backend now has proper CORS configuration:
- `allowCredentials: false` (required with wildcard origins)
- All methods allowed (GET, POST, PUT, PATCH, DELETE, OPTIONS, HEAD)
- All headers allowed
- Preflight caching enabled (1 hour)

## Quick Fixes

### Option 1: Frontend - Remove Credentials (Recommended)

```javascript
// In your axios configuration file
import axios from 'axios';

const apiClient = axios.create({
  baseURL: 'https://tbs-crm-dsfneefxf5bbbnad.centralindia-01.azurewebsites.net',
  withCredentials: false,  // Make sure this is false
  headers: {
    'Content-Type': 'application/json',
  },
});
```

### Option 2: Frontend - Use Proxy (Development Only)

If you're in development, use a proxy to avoid CORS:

```javascript
// vite.config.js
export default {
  server: {
    proxy: {
      '/api': {
        target: 'https://tbs-crm-dsfneefxf5bbbnad.centralindia-01.azurewebsites.net',
        changeOrigin: true,
      }
    }
  }
}
```

### Option 3: Backend - Specify Exact Origins (Production)

If you need credentials, specify exact origins instead of wildcard:

```java
// In CorsConfig.java
config.setAllowCredentials(true);
config.setAllowedOrigins(Arrays.asList(
    "https://tbscrm-frontend-cjcyene4bvc3d2gs.canadacentral-01.azurewebsites.net"
));
```

## Testing

1. **Check browser console** - Look for the exact CORS error message
2. **Check Network tab** - Look at the preflight OPTIONS request
3. **Check request headers** - See if `credentials: include` is being sent
4. **Test with curl** - Verify backend is responding correctly:

```bash
curl -X OPTIONS \
  -H "Origin: https://tbscrm-frontend-cjcyene4bvc3d2gs.canadacentral-01.azurewebsites.net" \
  -H "Access-Control-Request-Method: GET" \
  -H "Access-Control-Request-Headers: authorization" \
  -v \
  https://tbs-crm-dsfneefxf5bbbnad.centralindia-01.azurewebsites.net/api/deals
```

You should see:
```
< Access-Control-Allow-Origin: *
< Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE, OPTIONS, HEAD
< Access-Control-Allow-Headers: *
```

## Most Common Issue

**99% of the time**, the issue is that the frontend started sending `withCredentials: true` or `credentials: 'include'` in the requests. 

**Solution:** Remove it from your frontend code.

