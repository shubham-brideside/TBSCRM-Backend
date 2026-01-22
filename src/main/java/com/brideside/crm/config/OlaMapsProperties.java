package com.brideside.crm.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties(prefix = "ola.maps")
public class OlaMapsProperties {

    /**
     * Flag to enable/disable OLA Maps API integration
     */
    private boolean enabled = true;

    /**
     * OLA Maps API base URL
     */
    private String baseUrl = "https://api.olamaps.io";

    /**
     * OAuth 2.0 client ID (required)
     */
    private String clientId;

    /**
     * OAuth 2.0 client secret (required)
     */
    private String clientSecret;

    /**
     * Request timeout in milliseconds
     */
    private int timeout = 10000;

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public String getBaseUrl() {
        return baseUrl;
    }

    public void setBaseUrl(String baseUrl) {
        this.baseUrl = baseUrl;
    }

    public String getClientId() {
        return clientId;
    }

    public void setClientId(String clientId) {
        this.clientId = clientId;
    }

    public String getClientSecret() {
        return clientSecret;
    }

    public void setClientSecret(String clientSecret) {
        this.clientSecret = clientSecret;
    }

    public int getTimeout() {
        return timeout;
    }

    public void setTimeout(int timeout) {
        this.timeout = timeout;
    }
}

