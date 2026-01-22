package com.brideside.crm.integration.places;

import com.brideside.crm.config.OlaMapsProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

/**
 * Service for interacting with OLA Maps Places Autocomplete API.
 * Uses OAuth2 Client Credentials flow for authentication.
 */
@Service
public class OlaMapsService {

    private static final Logger log = LoggerFactory.getLogger(OlaMapsService.class);
    private static final String AUTOCOMPLETE_ENDPOINT = "/places/v1/autocomplete";

    private final OlaMapsProperties properties;
    private final OlaAuthService oauth2Service;
    private final RestTemplate restTemplate;

    public OlaMapsService(OlaMapsProperties properties, OlaAuthService oauth2Service) {
        this.properties = properties;
        this.oauth2Service = oauth2Service;
        this.restTemplate = new RestTemplate();
    }

    /**
     * Searches for venues using OLA Maps Autocomplete API.
     *
     * @param input The search query text
     * @return Raw JSON response from OLA Maps API as String
     * @throws RuntimeException if the API call fails
     */
    public String autocompleteVenues(String input) {
        if (!properties.isEnabled()) {
            log.warn("OLA Maps API is disabled");
            throw new IllegalStateException("OLA Maps API is disabled");
        }

        if (!StringUtils.hasText(input)) {
            log.warn("Empty input provided for autocomplete");
            throw new IllegalArgumentException("Input parameter is required and cannot be empty");
        }

        try {
            // Get OAuth2 access token
            String accessToken = oauth2Service.getAccessToken();

            // Build URL
            String url = buildAutocompleteUrl(input);
            log.debug("Calling OLA Maps Autocomplete API: {}", url);

            // Prepare request with Bearer token
            HttpHeaders headers = new HttpHeaders();
            headers.set("Authorization", "Bearer " + accessToken);
            headers.set("Accept", MediaType.APPLICATION_JSON_VALUE);
            HttpEntity<?> request = new HttpEntity<>(headers);

            // Make API call
            ResponseEntity<String> response = restTemplate.exchange(
                url,
                HttpMethod.GET,
                request,
                String.class
            );

            if (response.getStatusCode().is2xxSuccessful()) {
                log.debug("OLA Maps API call successful");
                return response.getBody();
            } else {
                log.error("OLA Maps API returned non-2xx status: {}", response.getStatusCode());
                throw new RuntimeException("OLA Maps API error: " + response.getStatusCode());
            }

        } catch (HttpClientErrorException.Unauthorized e) {
            log.error("OLA Maps API returned 401 Unauthorized. Invalidating token cache.");
            oauth2Service.invalidateToken();
            throw new RuntimeException("OLA Maps API authentication failed: " + e.getResponseBodyAsString(), e);
        } catch (HttpClientErrorException.Forbidden e) {
            log.error("OLA Maps API returned 403 Forbidden: {}", e.getResponseBodyAsString());
            throw new RuntimeException("OLA Maps API access forbidden: " + e.getResponseBodyAsString(), e);
        } catch (HttpClientErrorException e) {
            log.error("OLA Maps API returned error status {}: {}", e.getStatusCode(), e.getResponseBodyAsString());
            throw new RuntimeException("OLA Maps API error: " + e.getStatusCode() + " - " + e.getResponseBodyAsString(), e);
        } catch (RestClientException e) {
            log.error("Error calling OLA Maps API: {}", e.getMessage(), e);
            throw new RuntimeException("Failed to call OLA Maps API: " + e.getMessage(), e);
        } catch (Exception e) {
            log.error("Unexpected error in OLA Maps API call: {}", e.getMessage(), e);
            throw new RuntimeException("Unexpected error: " + e.getMessage(), e);
        }
    }

    /**
     * Builds the autocomplete API URL with query parameters.
     */
    private String buildAutocompleteUrl(String input) {
        return properties.getBaseUrl()
            + AUTOCOMPLETE_ENDPOINT
            + "?input=" + URLEncoder.encode(input.trim(), StandardCharsets.UTF_8);
    }
}
