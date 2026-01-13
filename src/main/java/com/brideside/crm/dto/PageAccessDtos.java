package com.brideside.crm.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * DTOs for page access management
 */
public class PageAccessDtos {
    
    /**
     * Response DTO for page access
     */
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PageAccessResponse {
        private Long userId;
        private Map<String, Boolean> pageAccess;
        
        public Long getUserId() {
            return userId;
        }
        
        public void setUserId(Long userId) {
            this.userId = userId;
        }
        
        public Map<String, Boolean> getPageAccess() {
            return pageAccess;
        }
        
        public void setPageAccess(Map<String, Boolean> pageAccess) {
            this.pageAccess = pageAccess;
        }
    }
    
    /**
     * Request DTO for bulk page access update
     */
    @NoArgsConstructor
    @AllArgsConstructor
    public static class UpdatePageAccessRequest {
        @NotNull(message = "Page access map is required")
        private Map<String, Boolean> pageAccess;
        
        public Map<String, Boolean> getPageAccess() {
            return pageAccess;
        }
        
        public void setPageAccess(Map<String, Boolean> pageAccess) {
            this.pageAccess = pageAccess;
        }
    }
    
    /**
     * Request DTO for single page access update
     */
    @NoArgsConstructor
    @AllArgsConstructor
    public static class UpdateSinglePageAccessRequest {
        @NotNull(message = "hasAccess is required")
        private Boolean hasAccess;
        
        public Boolean getHasAccess() {
            return hasAccess;
        }
        
        public void setHasAccess(Boolean hasAccess) {
            this.hasAccess = hasAccess;
        }
    }
    
    /**
     * Response DTO for single page access update
     */
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SinglePageAccessResponse {
        private Long userId;
        private String pageName;
        private Boolean hasAccess;
        
        public Long getUserId() {
            return userId;
        }
        
        public void setUserId(Long userId) {
            this.userId = userId;
        }
        
        public String getPageName() {
            return pageName;
        }
        
        public void setPageName(String pageName) {
            this.pageName = pageName;
        }
        
        public Boolean getHasAccess() {
            return hasAccess;
        }
        
        public void setHasAccess(Boolean hasAccess) {
            this.hasAccess = hasAccess;
        }
    }
    
    /**
     * Response DTO for page access summary (all users)
     */
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PageAccessSummaryResponse {
        private Long userId;
        private String email;
        private String firstName;
        private String lastName;
        private String role;
        private Map<String, Boolean> pageAccess;
        
        public Long getUserId() {
            return userId;
        }
        
        public void setUserId(Long userId) {
            this.userId = userId;
        }
        
        public String getEmail() {
            return email;
        }
        
        public void setEmail(String email) {
            this.email = email;
        }
        
        public String getFirstName() {
            return firstName;
        }
        
        public void setFirstName(String firstName) {
            this.firstName = firstName;
        }
        
        public String getLastName() {
            return lastName;
        }
        
        public void setLastName(String lastName) {
            this.lastName = lastName;
        }
        
        public String getRole() {
            return role;
        }
        
        public void setRole(String role) {
            this.role = role;
        }
        
        public Map<String, Boolean> getPageAccess() {
            return pageAccess;
        }
        
        public void setPageAccess(Map<String, Boolean> pageAccess) {
            this.pageAccess = pageAccess;
        }
    }
}

