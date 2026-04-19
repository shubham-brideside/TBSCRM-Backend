package com.brideside.crm.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;

public class UpdateUserRequest {
    
    @NotBlank(message = "Email is required")
    @Email(message = "Email should be valid")
    private String email;
    
    @NotBlank(message = "First name is required")
    private String firstName;
    
    @NotBlank(message = "Last name is required")
    private String lastName;
    
    @NotBlank(message = "Role is required")
    private String role;
    
    private Long managerId; // Optional: for reassigning manager in hierarchy

    /**
     * When role is CATEGORY_MANAGER: {@code categories.id}. Send a value to set; omit this property to keep
     * the existing value. To clear, use {@code PATCH /api/users/{id}/managed-category} with null.
     */
    @Schema(description = "categories.id for CATEGORY_MANAGER vertical scope; omit to leave unchanged")
    private Long managedCategoryId;

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
}

