package com.brideside.crm.dto;

import com.brideside.crm.entity.Organization.OrganizationCategory;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;

import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public final class OrganizationDtos {

    private OrganizationDtos() {
    }

    public static class OrganizationRequest {
        @NotBlank(message = "Organization name is required")
        @Size(max = 255, message = "Organization name cannot exceed 255 characters")
        private String name;

        @NotNull(message = "Organization category is required")
        private OrganizationCategory category;

        @Positive(message = "Owner id must be positive")
        private Long ownerId;

        @Size(max = 500, message = "Address cannot exceed 500 characters")
        private String address;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public OrganizationCategory getCategory() {
            return category;
        }

        public void setCategory(OrganizationCategory category) {
            this.category = category;
        }

        public Long getOwnerId() {
            return ownerId;
        }

        public void setOwnerId(Long ownerId) {
            this.ownerId = ownerId;
        }

        public String getAddress() {
            return address;
        }

        public void setAddress(String address) {
            this.address = address;
        }
    }

    public static class OrganizationResponse {
        private Long id;
        private String name;
        private OwnerSummary owner;
        private OrganizationCategory category;
        private String address;
        private Instant createdAt;
        private Instant updatedAt;

        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public OwnerSummary getOwner() {
            return owner;
        }

        public void setOwner(OwnerSummary owner) {
            this.owner = owner;
        }

        public OrganizationCategory getCategory() {
            return category;
        }

        public void setCategory(OrganizationCategory category) {
            this.category = category;
        }

        public String getAddress() {
            return address;
        }

        public void setAddress(String address) {
            this.address = address;
        }

        public Instant getCreatedAt() {
            return createdAt;
        }

        public void setCreatedAt(Instant createdAt) {
            this.createdAt = createdAt;
        }

        public Instant getUpdatedAt() {
            return updatedAt;
        }

        public void setUpdatedAt(Instant updatedAt) {
            this.updatedAt = updatedAt;
        }
    }

    public static class OwnerSummary {
        private Long id;
        private String firstName;
        private String lastName;
        private String email;
        private String role;

        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
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

        public String getEmail() {
            return email;
        }

        public void setEmail(String email) {
            this.email = email;
        }

        public String getRole() {
            return role;
        }

        public void setRole(String role) {
            this.role = role;
        }

        public String getDisplayName() {
            String first = firstName != null ? firstName : "";
            String last = lastName != null ? lastName : "";
            return (first + " " + last).trim();
        }
    }

    public static class OwnerOption extends OwnerSummary {
    }

    public static class CategoryOption {
        private String code;
        private String label;

        public CategoryOption() {
        }

        public CategoryOption(String code, String label) {
            this.code = code;
            this.label = label;
        }

        public String getCode() {
            return code;
        }

        public void setCode(String code) {
            this.code = code;
        }

        public String getLabel() {
            return label;
        }

        public void setLabel(String label) {
            this.label = label;
        }
    }

    public static List<CategoryOption> allCategoryOptions() {
        return Arrays.stream(OrganizationCategory.values())
                .map(cat -> new CategoryOption(cat.name(), cat.getDbValue()))
                .collect(Collectors.toList());
    }
}
