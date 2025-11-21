package com.brideside.crm.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

@Component
@ConfigurationProperties(prefix = "google.calendar")
public class GoogleCalendarProperties {

    /**
     * Flag to toggle Google Calendar sync without touching the code.
     */
    private boolean enabled = false;

    /**
     * Human readable app name that will appear inside Google Calendar.
     */
    private String applicationName = "Brideside CRM";

    /**
     * Absolute path to the service account JSON file. Optional when the raw JSON is provided.
     */
    private String credentialsFile;

    /**
     * Raw JSON content (or base64 decoded content) for the service account.
     */
    private String credentialsJson;

    /**
     * Optional user to impersonate when the service account needs domain-wide delegation.
     */
    private String impersonatedUser;

    /**
     * Polling cadence in minutes.
     */
    private long pollIntervalMinutes = 10;

    /**
     * Number of days in the past to pull events when syncing.
     */
    private int syncWindowPastDays = 30;

    /**
     * Number of days in the future to pull events when syncing.
     */
    private int syncWindowFutureDays = 120;

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public String getApplicationName() {
        return applicationName;
    }

    public void setApplicationName(String applicationName) {
        this.applicationName = applicationName;
    }

    public String getCredentialsFile() {
        return credentialsFile;
    }

    public void setCredentialsFile(String credentialsFile) {
        this.credentialsFile = credentialsFile;
    }

    public String getCredentialsJson() {
        return credentialsJson;
    }

    public void setCredentialsJson(String credentialsJson) {
        this.credentialsJson = credentialsJson;
    }

    public String getImpersonatedUser() {
        return impersonatedUser;
    }

    public void setImpersonatedUser(String impersonatedUser) {
        this.impersonatedUser = impersonatedUser;
    }

    public long getPollIntervalMinutes() {
        return pollIntervalMinutes;
    }

    public void setPollIntervalMinutes(long pollIntervalMinutes) {
        this.pollIntervalMinutes = pollIntervalMinutes;
    }

    public int getSyncWindowPastDays() {
        return syncWindowPastDays;
    }

    public void setSyncWindowPastDays(int syncWindowPastDays) {
        this.syncWindowPastDays = syncWindowPastDays;
    }

    public int getSyncWindowFutureDays() {
        return syncWindowFutureDays;
    }

    public void setSyncWindowFutureDays(int syncWindowFutureDays) {
        this.syncWindowFutureDays = syncWindowFutureDays;
    }

    public boolean hasCredentialSource() {
        return StringUtils.hasText(credentialsFile) || StringUtils.hasText(credentialsJson);
    }
}

