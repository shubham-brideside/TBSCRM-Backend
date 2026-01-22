package com.brideside.crm.integration.places;

import com.brideside.crm.config.OlaMapsProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.util.StringUtils;
import org.springframework.web.client.RestTemplate;

import java.time.Instant;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Service for managing OAuth2 Client Credentials flow with OLA Maps API.
 * Handles token acquisition, caching, and automatic refresh.
 * 
 * Thread-safe implementation with automatic token refresh before expiry.
 */
@Service
public class OlaAuthService {

    private static final Logger log = LoggerFactory.getLogger(OlaAuthService.class);
    private static final String TOKEN_ENDPOINT = "/auth/v1/token";
    private static final String GRANT_TYPE = "client_credentials";
    private static final String SCOPE = "openid";
    
    // Refresh token 5 minutes before expiry
    private static final long TOKEN_REFRESH_BUFFER_SECONDS = 300;

    private final OlaMapsProperties properties;
    private final RestTemplate restTemplate;
    private final ReentrantLock tokenLock = new ReentrantLock();

    // Cached token data
    private volatile String cachedAccessToken;
    private volatile Instant tokenExpiryTime;

    public OlaAuthService(OlaMapsProperties properties) {
        this.properties = properties;
        this.restTemplate = new RestTemplate();
    }

    /**
     * Gets a valid access token, refreshing if necessary.
     * Thread-safe with automatic token refresh.
     *
     * @return Bearer access token
     * @throws RuntimeException if token acquisition fails
     */
    public String getAccessToken() {
        // Fast path: check if we have a valid cached token
        if (isTokenValid()) {
            return cachedAccessToken;
        }

        // Slow path: acquire lock and refresh token
        tokenLock.lock();
        try {
            // Double-check after acquiring lock
            if (isTokenValid()) {
                return cachedAccessToken;
            }

            log.info("Acquiring new OAuth2 access token from OLA Maps API");
            TokenResponse tokenResponse = acquireToken();
            
            cachedAccessToken = tokenResponse.getAccessToken();
            // Calculate expiry time (default to 1 hour if expires_in not provided)
            long expiresIn = tokenResponse.getExpiresIn() != null 
                ? tokenResponse.getExpiresIn() 
                : 3600;
            tokenExpiryTime = Instant.now().plusSeconds(expiresIn - TOKEN_REFRESH_BUFFER_SECONDS);
            
            log.info("Successfully acquired OAuth2 access token, expires in {} seconds", expiresIn);
            return cachedAccessToken;

        } catch (Exception e) {
            log.error("Failed to acquire OAuth2 access token: {}", e.getMessage(), e);
            throw new RuntimeException("Failed to acquire OAuth2 access token: " + e.getMessage(), e);
        } finally {
            tokenLock.unlock();
        }
    }

    /**
     * Checks if the cached token is still valid.
     */
    private boolean isTokenValid() {
        return cachedAccessToken != null 
            && tokenExpiryTime != null 
            && Instant.now().isBefore(tokenExpiryTime);
    }

    /**
     * Acquires a new access token from OLA Maps OAuth2 endpoint.
     */
    private TokenResponse acquireToken() {
        if (!StringUtils.hasText(properties.getClientId()) || !StringUtils.hasText(properties.getClientSecret())) {
            throw new IllegalStateException(
                "OLA Maps OAuth2 credentials not configured. Please set OLA_MAPS_CLIENT_ID and OLA_MAPS_CLIENT_SECRET."
            );
        }

        String tokenUrl = properties.getBaseUrl() + TOKEN_ENDPOINT;

        // Prepare form data
        MultiValueMap<String, String> formData = new LinkedMultiValueMap<>();
        formData.add("grant_type", GRANT_TYPE);
        formData.add("scope", SCOPE);
        formData.add("client_id", properties.getClientId());
        formData.add("client_secret", properties.getClientSecret());

        // Set headers
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

        HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(formData, headers);

        try {
            ResponseEntity<TokenResponse> response = restTemplate.exchange(
                tokenUrl,
                HttpMethod.POST,
                request,
                TokenResponse.class
            );

            if (response.getStatusCode().is2xxSuccessful() && response.getBody() != null) {
                return response.getBody();
            } else {
                throw new RuntimeException("OAuth2 token request failed with status: " + response.getStatusCode());
            }
        } catch (org.springframework.web.client.HttpClientErrorException.Unauthorized e) {
            log.error("OAuth2 token request failed with 401 Unauthorized: {}", e.getResponseBodyAsString());
            throw new RuntimeException("OAuth2 authentication failed: Invalid client credentials. " + e.getResponseBodyAsString(), e);
        } catch (org.springframework.web.client.HttpClientErrorException.Forbidden e) {
            log.error("OAuth2 token request failed with 403 Forbidden: {}", e.getResponseBodyAsString());
            throw new RuntimeException("OAuth2 authentication failed: Access forbidden. " + e.getResponseBodyAsString(), e);
        } catch (org.springframework.web.client.RestClientException e) {
            log.error("OAuth2 token request failed: {}", e.getMessage(), e);
            throw new RuntimeException("OAuth2 token acquisition failed: " + e.getMessage(), e);
        } catch (Exception e) {
            log.error("Unexpected error during OAuth2 token acquisition: {}", e.getMessage(), e);
            throw new RuntimeException("OAuth2 token acquisition failed: " + e.getMessage(), e);
        }
    }

    /**
     * Invalidates the cached token, forcing a refresh on next request.
     */
    public void invalidateToken() {
        tokenLock.lock();
        try {
            cachedAccessToken = null;
            tokenExpiryTime = null;
            log.info("OAuth2 access token cache invalidated");
        } finally {
            tokenLock.unlock();
        }
    }

    /**
     * DTO for OAuth2 token response.
     */
    public static class TokenResponse {
        @JsonProperty("access_token")
        private String accessToken;

        @JsonProperty("token_type")
        private String tokenType;

        @JsonProperty("expires_in")
        private Long expiresIn;

        @JsonProperty("scope")
        private String scope;

        public String getAccessToken() {
            return accessToken;
        }

        public void setAccessToken(String accessToken) {
            this.accessToken = accessToken;
        }

        public String getTokenType() {
            return tokenType;
        }

        public void setTokenType(String tokenType) {
            this.tokenType = tokenType;
        }

        public Long getExpiresIn() {
            return expiresIn;
        }

        public void setExpiresIn(Long expiresIn) {
            this.expiresIn = expiresIn;
        }

        public String getScope() {
            return scope;
        }

        public void setScope(String scope) {
            this.scope = scope;
        }
    }
}

