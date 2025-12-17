package com.brideside.crm.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public class LabelDtos {

    @Schema(description = "Request to create a new label")
    public static class CreateLabelRequest {
        @NotBlank(message = "Label name is required")
        @Size(max = 100, message = "Label name must not exceed 100 characters")
        @Schema(description = "Name of the label", example = "IMPORTANT", requiredMode = Schema.RequiredMode.REQUIRED)
        private String name;

        @Size(max = 20, message = "Color must not exceed 20 characters")
        @Schema(description = "Color of the label in hex format", example = "#f59e0b", defaultValue = "#94a3b8")
        private String color = "#94a3b8";

        @Schema(description = "Organization ID the label belongs to")
        private Long organizationId;

        @Schema(description = "Pipeline ID the label belongs to")
        private Long pipelineId;

        @Schema(description = "User ID who created the label")
        private Long createdByUserId;

        @Schema(description = "Whether the label is global (available across all organizations/pipelines)", defaultValue = "false")
        private Boolean isGlobal = false;

        // Getters and Setters
        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getColor() {
            return color;
        }

        public void setColor(String color) {
            this.color = color;
        }

        public Long getOrganizationId() {
            return organizationId;
        }

        public void setOrganizationId(Long organizationId) {
            this.organizationId = organizationId;
        }

        public Long getPipelineId() {
            return pipelineId;
        }

        public void setPipelineId(Long pipelineId) {
            this.pipelineId = pipelineId;
        }

        public Long getCreatedByUserId() {
            return createdByUserId;
        }

        public void setCreatedByUserId(Long createdByUserId) {
            this.createdByUserId = createdByUserId;
        }

        public Boolean getIsGlobal() {
            return isGlobal;
        }

        public void setIsGlobal(Boolean isGlobal) {
            this.isGlobal = isGlobal;
        }
    }

    @Schema(description = "Request to update an existing label")
    public static class UpdateLabelRequest {
        @Size(max = 100, message = "Label name must not exceed 100 characters")
        @Schema(description = "Name of the label", example = "URGENT")
        private String name;

        @Size(max = 20, message = "Color must not exceed 20 characters")
        @Schema(description = "Color of the label in hex format", example = "#ef4444")
        private String color;

        @Schema(description = "Organization ID the label belongs to")
        private Long organizationId;

        @Schema(description = "Pipeline ID the label belongs to")
        private Long pipelineId;

        @Schema(description = "Whether the label is global")
        private Boolean isGlobal;

        // Getters and Setters
        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getColor() {
            return color;
        }

        public void setColor(String color) {
            this.color = color;
        }

        public Long getOrganizationId() {
            return organizationId;
        }

        public void setOrganizationId(Long organizationId) {
            this.organizationId = organizationId;
        }

        public Long getPipelineId() {
            return pipelineId;
        }

        public void setPipelineId(Long pipelineId) {
            this.pipelineId = pipelineId;
        }

        public Boolean getIsGlobal() {
            return isGlobal;
        }

        public void setIsGlobal(Boolean isGlobal) {
            this.isGlobal = isGlobal;
        }
    }

    @Schema(description = "Label response")
    public static class LabelResponse {
        @Schema(description = "Unique identifier of the label")
        private Long id;

        @Schema(description = "Name of the label")
        private String name;

        @Schema(description = "Color of the label in hex format")
        private String color;

        @Schema(description = "Organization ID the label belongs to")
        private Long organizationId;

        @Schema(description = "Pipeline ID the label belongs to")
        private Long pipelineId;

        @Schema(description = "User ID who created the label")
        private Long createdByUserId;

        @Schema(description = "Whether the label is global")
        private Boolean isGlobal;

        @Schema(description = "Timestamp when the label was created")
        private String createdAt;

        @Schema(description = "Timestamp when the label was last updated")
        private String updatedAt;

        // Getters and Setters
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

        public String getColor() {
            return color;
        }

        public void setColor(String color) {
            this.color = color;
        }

        public Long getOrganizationId() {
            return organizationId;
        }

        public void setOrganizationId(Long organizationId) {
            this.organizationId = organizationId;
        }

        public Long getPipelineId() {
            return pipelineId;
        }

        public void setPipelineId(Long pipelineId) {
            this.pipelineId = pipelineId;
        }

        public Long getCreatedByUserId() {
            return createdByUserId;
        }

        public void setCreatedByUserId(Long createdByUserId) {
            this.createdByUserId = createdByUserId;
        }

        public Boolean getIsGlobal() {
            return isGlobal;
        }

        public void setIsGlobal(Boolean isGlobal) {
            this.isGlobal = isGlobal;
        }

        public String getCreatedAt() {
            return createdAt;
        }

        public void setCreatedAt(String createdAt) {
            this.createdAt = createdAt;
        }

        public String getUpdatedAt() {
            return updatedAt;
        }

        public void setUpdatedAt(String updatedAt) {
            this.updatedAt = updatedAt;
        }
    }
}
