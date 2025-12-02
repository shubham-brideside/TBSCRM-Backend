package com.brideside.crm.dto;

import com.brideside.crm.entity.Person;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import java.time.Instant;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

@Schema(description = "Person data transfer object")
public class PersonDTO {

    @Schema(description = "Person ID (auto-generated, omit for POST requests)", accessMode = Schema.AccessMode.READ_ONLY)
    private Long id;
    
    @NotBlank
    @Size(max = 255)
    @Schema(description = "Person name", required = true)
    private String name;
    
    @Schema(description = "Instagram ID / handle")
    private String instagramId;
    
    @Schema(description = "Phone number")
    private String phone;
    
    @Schema(description = "Email address")
    private String email;

    @Schema(description = "Lead date (yyyy-MM-dd)")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDate leadDate;

    @Schema(description = "Organization identifier")
    private Long organizationId;

    @Schema(description = "Organization name", accessMode = Schema.AccessMode.READ_ONLY)
    private String organizationName;

    @Schema(description = "Owner user identifier")
    private Long ownerId;

    @Schema(description = "Owner display name", accessMode = Schema.AccessMode.READ_ONLY)
    private String ownerDisplayName;

    @Schema(description = "Owner email", accessMode = Schema.AccessMode.READ_ONLY)
    private String ownerEmail;

    @Schema(description = "Category ID (FK to categories.id)", type = "integer", format = "int64")
    private Long categoryId;

    @Schema(description = "Category name", accessMode = Schema.AccessMode.READ_ONLY)
    private String categoryName;

    @Schema(description = "Lead label / category")
    private Person.PersonLabel label;

    @Schema(description = "Lead source")
    private com.brideside.crm.entity.DealSource source;

    @Schema(description = "Lead sub source (only when source is Direct)")
    private com.brideside.crm.entity.DealSubSource subSource;

    @Schema(description = "Record created timestamp", accessMode = Schema.AccessMode.READ_ONLY)
    private Instant createdAt;

    @Schema(description = "Record updated timestamp", accessMode = Schema.AccessMode.READ_ONLY)
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

    public String getInstagramId() {
        return instagramId;
    }

    public void setInstagramId(String instagramId) {
        this.instagramId = instagramId;
    }

    public String getPhone() {
        return phone;
    }

    public void setPhone(String phone) {
        this.phone = phone;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public LocalDate getLeadDate() {
        return leadDate;
    }

    public void setLeadDate(LocalDate leadDate) {
        this.leadDate = leadDate;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public String getOrganizationName() {
        return organizationName;
    }

    public void setOrganizationName(String organizationName) {
        this.organizationName = organizationName;
    }

    public Long getOwnerId() {
        return ownerId;
    }

    public void setOwnerId(Long ownerId) {
        this.ownerId = ownerId;
    }

    public String getOwnerDisplayName() {
        return ownerDisplayName;
    }

    public void setOwnerDisplayName(String ownerDisplayName) {
        this.ownerDisplayName = ownerDisplayName;
    }

    public String getOwnerEmail() {
        return ownerEmail;
    }

    public void setOwnerEmail(String ownerEmail) {
        this.ownerEmail = ownerEmail;
    }

    public Long getCategoryId() {
        return categoryId;
    }

    public void setCategoryId(Long categoryId) {
        this.categoryId = categoryId;
    }

    public String getCategoryName() {
        return categoryName;
    }

    public void setCategoryName(String categoryName) {
        this.categoryName = categoryName;
    }

    public Person.PersonLabel getLabel() {
        return label;
    }

    public void setLabel(Person.PersonLabel label) {
        this.label = label;
    }

    public com.brideside.crm.entity.DealSource getSource() {
        return source;
    }

    public void setSource(com.brideside.crm.entity.DealSource source) {
        this.source = source;
    }

    public com.brideside.crm.entity.DealSubSource getSubSource() {
        return subSource;
    }

    public void setSubSource(com.brideside.crm.entity.DealSubSource subSource) {
        this.subSource = subSource;
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

    public static class OwnerOption {
        private Long id;
        private String firstName;
        private String lastName;
        private String email;

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

        public String getDisplayName() {
            StringBuilder builder = new StringBuilder();
            if (firstName != null) builder.append(firstName);
            if (lastName != null && !lastName.isBlank()) {
                if (!builder.isEmpty()) builder.append(" ");
                builder.append(lastName);
            }
            return builder.isEmpty() ? email : builder.toString();
        }
    }

    public static class EnumOption {
        private String code;
        private String label;

        public EnumOption() {
        }

        public EnumOption(String code, String label) {
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

    public static List<EnumOption> labelOptions() {
        return Arrays.stream(Person.PersonLabel.values())
                .map(v -> new EnumOption(v.name(), v.getDisplayName()))
                .collect(Collectors.toList());
    }

}

