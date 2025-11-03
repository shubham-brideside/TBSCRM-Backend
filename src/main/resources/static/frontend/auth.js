// Authentication utility functions

const API_BASE_URL = 'http://localhost:8080/api';

/**
 * Check if JWT token is expired
 * @param {string} token - JWT token
 * @returns {boolean} - true if token is expired or invalid
 */
function isTokenExpired(token) {
    if (!token) {
        return true;
    }
    
    try {
        // JWT format: header.payload.signature
        const parts = token.split('.');
        if (parts.length !== 3) {
            return true;
        }
        
        // Decode payload (base64)
        const payload = JSON.parse(atob(parts[1]));
        
        // Check expiration
        if (!payload.exp) {
            return true;
        }
        
        // exp is in seconds, Date.now() is in milliseconds
        const expirationTime = payload.exp * 1000;
        const currentTime = Date.now();
        
        // Add 5 second buffer to handle network delays
        return currentTime >= (expirationTime - 5000);
    } catch (error) {
        console.error('Error checking token expiration:', error);
        return true;
    }
}

/**
 * Logout user and redirect to login page
 */
function logout() {
    localStorage.removeItem('token');
    localStorage.removeItem('user');
    window.location.href = '/frontend/login.html';
}

/**
 * Check if user is authenticated and token is valid
 * If not, redirect to login
 */
function checkAuthentication() {
    const token = localStorage.getItem('token');
    
    if (!token) {
        logout();
        return false;
    }
    
    if (isTokenExpired(token)) {
        alert('Your session has expired. Please login again.');
        logout();
        return false;
    }
    
    return true;
}

/**
 * Wrapper for fetch API that handles authentication and token expiration
 * @param {string} url - API endpoint
 * @param {object} options - Fetch options
 * @returns {Promise<Response>}
 */
async function authenticatedFetch(url, options = {}) {
    const token = localStorage.getItem('token');
    
    console.log('authenticatedFetch called - URL:', url);
    console.log('authenticatedFetch - Token present:', !!token);
    
    // Check if token exists and is not expired
    if (!token || isTokenExpired(token)) {
        console.log('authenticatedFetch - Token missing or expired');
        alert('Your session has expired. Please login again.');
        logout();
        return Promise.reject(new Error('Token expired'));
    }
    
    // Merge headers - ensure we don't override existing headers
    const headers = new Headers();
    
    // Add default headers
    headers.set('Authorization', `Bearer ${token}`);
    
    // Add Content-Type only if not already set and body exists
    if (options.body && !options.headers?.['Content-Type']) {
        headers.set('Content-Type', 'application/json');
    }
    
    // Merge any existing headers from options
    if (options.headers) {
        if (options.headers instanceof Headers) {
            options.headers.forEach((value, key) => {
                headers.set(key, value);
            });
        } else {
            Object.keys(options.headers).forEach(key => {
                headers.set(key, options.headers[key]);
            });
        }
    }
    
    console.log('authenticatedFetch - Authorization header:', headers.get('Authorization') ? 'Present' : 'Missing');
    
    try {
        const fetchOptions = {
            ...options,
            headers: headers
        };
        
        console.log('authenticatedFetch - Making request with options:', {
            method: fetchOptions.method || 'GET',
            url: url,
            hasAuthorization: !!headers.get('Authorization')
        });
        
        const response = await fetch(url, fetchOptions);
        
        // Handle 401 (Unauthorized) or 403 (Forbidden) responses
        if (response.status === 401 || response.status === 403) {
            const errorData = await response.json().catch(() => ({ message: 'Unauthorized' }));
            
            // Token expired or invalid
            if (response.status === 401) {
                alert('Your session has expired. Please login again.');
            } else {
                alert('Access denied. Please login again.');
            }
            
            logout();
            return Promise.reject(new Error(errorData.message || 'Unauthorized'));
        }
        
        return response;
    } catch (error) {
        // Network errors or other issues
        console.error('API request failed:', error);
        throw error;
    }
}

/**
 * Get current user from localStorage
 * @returns {object|null} - User object or null
 */
function getCurrentUser() {
    const userStr = localStorage.getItem('user');
    if (!userStr) {
        return null;
    }
    
    try {
        return JSON.parse(userStr);
    } catch (error) {
        console.error('Error parsing user data:', error);
        return null;
    }
}

/**
 * Show global loader overlay
 * @param {string} message - Optional message to display
 */
function showLoader(message = 'Loading...') {
    // Remove existing loader if any
    hideLoader();
    
    const overlay = document.createElement('div');
    overlay.className = 'loader-overlay';
    overlay.id = 'globalLoader';
    overlay.innerHTML = `
        <div class="loader-container">
            <div class="loader-spinner"></div>
            <p class="loader-text">${message}</p>
        </div>
    `;
    document.body.appendChild(overlay);
}

/**
 * Hide global loader overlay
 */
function hideLoader() {
    const loader = document.getElementById('globalLoader');
    if (loader) {
        loader.remove();
    }
}

/**
 * Set button loading state
 * @param {HTMLElement} button - Button element
 * @param {boolean} loading - Whether to show loading state
 */
function setButtonLoading(button, loading) {
    if (loading) {
        button.classList.add('loading');
        button.disabled = true;
    } else {
        button.classList.remove('loading');
        button.disabled = false;
    }
}

