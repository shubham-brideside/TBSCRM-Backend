package com.brideside.crm.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public class CreateUserRequest {
    
    @NotBlank(message = "Email is required")
    @Email(message = "Email should be valid")
    private String email;
    
    @NotBlank(message = "First name is required")
    private String firstName;
    
    @NotBlank(message = "Last name is required")
    private String lastName;
    
    @NotBlank(message = "Role is required")
    private String role;
    
    private Long managerId; // Optional: for assigning manager in hierarchy

    /** When role is CATEGORY_MANAGER, {@code categories.id} for pipeline/deal visibility (e.g. Photography). */
    private Long managedCategoryId;

    /**
     * When true, this user is created via the TBS flow: {@code role} must be one of
     * {@code TBS_PRESALES}, {@code TBS_REL_MANAGER}, or {@code TBS_SVC_MANAGER},
     * and a default pipeline is created (see API docs).
     */
    private Boolean isTbsUser;

    /**
     * Required when {@code role} is {@code TBS_SVC_MANAGER}: organization id must be one of
     * Revaah, TBS Planning, or TBS Venue (configurable via {@code app.tbs.user-onboarding.*}).
     */
    private Long tbsOrganizationId;

    // Getters and Setters
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

    public Long getManagerId() {
        return managerId;
    }

    public void setManagerId(Long managerId) {
        this.managerId = managerId;
    }

    public Long getManagedCategoryId() {
        return managedCategoryId;
    }

    public void setManagedCategoryId(Long managedCategoryId) {
        this.managedCategoryId = managedCategoryId;
    }

    public Boolean getIsTbsUser() {
        return isTbsUser;
    }

    public void setIsTbsUser(Boolean isTbsUser) {
        this.isTbsUser = isTbsUser;
    }

    public Long getTbsOrganizationId() {
        return tbsOrganizationId;
    }

    public void setTbsOrganizationId(Long tbsOrganizationId) {
        this.tbsOrganizationId = tbsOrganizationId;
    }
}

