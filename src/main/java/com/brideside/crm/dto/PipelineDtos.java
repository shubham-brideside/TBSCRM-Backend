package com.brideside.crm.dto;

import jakarta.validation.constraints.*;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

public final class PipelineDtos {
    private PipelineDtos() {
    }

    public static class PipelineRequest {
        @NotBlank(message = "Pipeline name is required")
        @Size(max = 255, message = "Pipeline name cannot exceed 255 characters")
        private String name;

        @Size(max = 255, message = "Category cannot exceed 255 characters")
        private String category;

        private Long teamId;

        private Long organizationId;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getCategory() {
            return category;
        }

        public void setCategory(String category) {
            this.category = category;
        }

        public Long getTeamId() {
            return teamId;
        }

        public void setTeamId(Long teamId) {
            this.teamId = teamId;
        }

        public Long getOrganizationId() {
            return organizationId;
        }

        public void setOrganizationId(Long organizationId) {
            this.organizationId = organizationId;
        }
    }

    public static class PipelineUpdateRequest {
        @Size(max = 255, message = "Pipeline name cannot exceed 255 characters")
        private String name;

        @Size(max = 255, message = "Category cannot exceed 255 characters")
        private String category;

        private Long teamId;

        private Long organizationId;

        private Boolean deleted;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getCategory() {
            return category;
        }

        public void setCategory(String category) {
            this.category = category;
        }

        public Long getTeamId() {
            return teamId;
        }

        public void setTeamId(Long teamId) {
            this.teamId = teamId;
        }

        public Long getOrganizationId() {
            return organizationId;
        }

        public void setOrganizationId(Long organizationId) {
            this.organizationId = organizationId;
        }

        public Boolean getDeleted() {
            return deleted;
        }

        public void setDeleted(Boolean deleted) {
            this.deleted = deleted;
        }
    }

    public static class StageRequest {
        @NotBlank(message = "Stage name is required")
        @Size(max = 255, message = "Stage name cannot exceed 255 characters")
        private String name;

        @Min(value = 0, message = "Stage order cannot be negative")
        private Integer order;

        @Min(value = 0, message = "Probability must be between 0 and 100")
        @Max(value = 100, message = "Probability must be between 0 and 100")
        private Integer probability;

        private Boolean active;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public Integer getOrder() {
            return order;
        }

        public void setOrder(Integer order) {
            this.order = order;
        }

        public Integer getProbability() {
            return probability;
        }

        public void setProbability(Integer probability) {
            this.probability = probability;
        }

        public Boolean getActive() {
            return active;
        }

        public void setActive(Boolean active) {
            this.active = active;
        }

    }

    public static class StageUpdateRequest {
        @Size(max = 255, message = "Stage name cannot exceed 255 characters")
        private String name;

        @Min(value = 0, message = "Stage order cannot be negative")
        private Integer order;

        @Min(value = 0, message = "Probability must be between 0 and 100")
        @Max(value = 100, message = "Probability must be between 0 and 100")
        private Integer probability;

        private Boolean active;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public Integer getOrder() {
            return order;
        }

        public void setOrder(Integer order) {
            this.order = order;
        }

        public Integer getProbability() {
            return probability;
        }

        public void setProbability(Integer probability) {
            this.probability = probability;
        }

        public Boolean getActive() {
            return active;
        }

        public void setActive(Boolean active) {
            this.active = active;
        }

    }

    public static class StageOrderRequest {
        @NotEmpty(message = "Stage order list cannot be empty")
        private List<@NotNull(message = "Stage id cannot be null") Long> orderedStageIds = new ArrayList<>();

        public List<Long> getOrderedStageIds() {
            return orderedStageIds;
        }

        public void setOrderedStageIds(List<Long> orderedStageIds) {
            this.orderedStageIds = orderedStageIds;
        }
    }

    public static class PipelineResponse {
        private Long id;
        private String name;
        private String category;
        private Long teamId;
        private TeamSummary team;
        private OrganizationSummary organization;
        private Boolean deleted;
        private Instant createdAt;
        private Instant updatedAt;
        private List<StageResponse> stages = List.of();

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

        public String getCategory() {
            return category;
        }

        public void setCategory(String category) {
            this.category = category;
        }

        public Long getTeamId() {
            return teamId;
        }

        public void setTeamId(Long teamId) {
            this.teamId = teamId;
        }

        public TeamSummary getTeam() {
            return team;
        }

        public void setTeam(TeamSummary team) {
            this.team = team;
        }

        public OrganizationSummary getOrganization() {
            return organization;
        }

        public void setOrganization(OrganizationSummary organization) {
            this.organization = organization;
        }

        public Boolean getDeleted() {
            return deleted;
        }

        public void setDeleted(Boolean deleted) {
            this.deleted = deleted;
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

        public List<StageResponse> getStages() {
            return stages;
        }

        public void setStages(List<StageResponse> stages) {
            this.stages = stages;
        }
    }

    public static class OrganizationSummary {
        private Long id;
        private String name;

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
    }

    public static class TeamSummary {
        private Long id;
        private String name;

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

    public static java.util.List<CategoryOption> allCategoryOptions() {
        return java.util.List.of(
                new CategoryOption("PHOTOGRAPHY", "Photography"),
                new CategoryOption("MAKEUP", "Makeup"),
                new CategoryOption("PLANNING_AND_DECOR", "Planning and Decor")
        );
    }

    public static class StageResponse {
        private Long id;
        private Long pipelineId;
        private String name;
        private Integer order;
        private Integer probability;
        private Boolean active;
        private Instant createdAt;
        private Instant updatedAt;

        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public Long getPipelineId() {
            return pipelineId;
        }

        public void setPipelineId(Long pipelineId) {
            this.pipelineId = pipelineId;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public Integer getOrder() {
            return order;
        }

        public void setOrder(Integer order) {
            this.order = order;
        }

        public Integer getProbability() {
            return probability;
        }

        public void setProbability(Integer probability) {
            this.probability = probability;
        }

        public Boolean getActive() {
            return active;
        }

        public void setActive(Boolean active) {
            this.active = active;
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
}


