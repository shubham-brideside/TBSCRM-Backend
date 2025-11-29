package com.brideside.crm.dto;

import com.brideside.crm.entity.Activity;
import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.annotation.JsonSetter;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;

@Schema(description = "Activity data transfer object")
@JsonPropertyOrder({
        "id",
        "subject",
        "category",
        "type",
        "date",
        "dueDate",
        "startTime",
        "endTime",
        "dateTime",
        "priority",
        "assignedUser",
        "notes",
        "organization",
        "organizationId",
        "organizationCategory",
        "organizationOwnerId",
        "organizationOwnerName",
        "assignedUserId",
        "personId",
        "dealId",
        "dealName",
        "instagramId",
        "phone",
        "status",
        "done",
        "serviceCategory",
        "durationMinutes",
        "attachmentUrl"
})
public class ActivityDTO {
    @Schema(description = "Activity ID (auto-generated, omit for POST requests)", type = "integer", format = "int64", accessMode = Schema.AccessMode.READ_ONLY)
    private Long id;

    // Fields in exact order as specified for create request
    @NotBlank
    @Schema(description = "Activity subject/title (required, trimmed non-empty string)", required = true)
    private String subject;
    
    @Schema(description = "Category of the activity (ACTIVITY, CALL, MEETING_SCHEDULER). Defaults to ACTIVITY if not provided. Maps to UI tabs: Activity, Call, Meeting scheduler", 
            allowableValues = {"ACTIVITY", "CALL", "MEETING_SCHEDULER"}, example = "ACTIVITY")
    private Activity.ActivityCategory category;
    
    @Schema(description = "Activity type (ACTIVITY, CALL, MEETING, FOLLOW_UP, etc.)")
    private String type;
    
    @Schema(description = "Schedule date (format: dd/MM/yyyy)")
    private String date;
    
    @Schema(description = "Due date (format: dd/MM/yyyy). Mirrors date for filtering")
    private String dueDate;
    
    @Schema(description = "Start time (format: HH:mm). Optional")
    private String startTime;
    
    @Schema(description = "End time (format: HH:mm). Optional")
    private String endTime;
    
    @Schema(description = "Canonical timestamp (format: yyyy-MM-ddTHH:mm:ss). Optional, sent when date is provided")
    private String dateTime;
    
    @Schema(description = "Priority level (LOW, MEDIUM, HIGH). Uppercase", allowableValues = {"HIGH", "MEDIUM", "LOW"})
    private Activity.PriorityLevel priority;
    
    @Schema(description = "Assigned user (email/identifier)")
    private String assignedUser;
    
    @Schema(description = "Activity notes (multiline text). Optional")
    private String notes;
    
    @Schema(description = "Organization name. Optional (legacy display field)")
    private String organization;

    @Schema(description = "Organization ID (FK to organizations.id). Preferred for filters and scoping", type = "integer", format = "int64")
    private Long organizationId;

    @Schema(description = "Organization category (e.g. PHOTOGRAPHY, MAKEUP, PLANNING_AND_DECOR). Derived from the linked organization, if present")
    private String organizationCategory;

    @Schema(description = "Organization owner user ID. Derived from the linked organization, if present", type = "integer", format = "int64")
    private Long organizationOwnerId;

    @Schema(description = "Organization owner full name. Derived from the linked organization, if present")
    private String organizationOwnerName;

    @Schema(description = "Assigned user ID (FK to users.id). Preferred for filters and scoping", type = "integer", format = "int64")
    private Long assignedUserId;
    
    @Schema(description = "Person ID. Optional, only if > 0", type = "integer", format = "int64")
    private Long personId;
    
    @Schema(description = "Deal ID. Optional, only if > 0", type = "integer", format = "int64")
    private Long dealId;
    
    @Schema(description = "Deal name. Optional, for display")
    @JsonAlias({"deal", "dealOrLead", "dealOrLeadName", "dealName"})
    private String dealName;
    
    @Schema(description = "Instagram ID. Optional")
    private String instagramId;
    
    @Schema(description = "Phone number. Optional")
    private String phone;
    
    @Schema(description = "Status of the activity. Optional, pass-through", allowableValues = {"OPEN", "PENDING", "IN_PROGRESS", "COMPLETED", "CANCELLED"})
    private Activity.ActivityStatus status;

    @Schema(description = "Done status (not accepted in create request - use POST /api/activities/{id}/done to toggle)",
            defaultValue = "false", type = "boolean", accessMode = Schema.AccessMode.READ_ONLY)
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private Boolean done = false;
    
    @Schema(description = "Service category (PHOTOGRAPHY, MAKEUP, PLANNING_DECOR). Optional/future", allowableValues = {"PHOTOGRAPHY", "MAKEUP", "PLANNING_DECOR"})
    private String serviceCategory;
    
    @Schema(description = "Duration in minutes. Optional, for Call tab only", type = "integer")
    private Integer durationMinutes;
    
    @Schema(description = "Attachment URL. Optional/future, placeholder for Call tab")
    private String attachmentUrl;
    
    // Getters and setters in the same order as field declarations
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    
    public String getSubject() { return subject; }
    public void setSubject(String subject) { this.subject = subject; }
    
    public Activity.ActivityCategory getCategory() { return category; }
    public void setCategory(Activity.ActivityCategory category) { this.category = category; }
    @JsonSetter("category")
    public void setCategory(String category) {
        this.category = parseEnum(category, Activity.ActivityCategory.class);
    }
    
    public String getType() { return type; }
    public void setType(String type) { this.type = type; }
    
    public String getDate() { return date; }
    public void setDate(String date) { this.date = date; }
    
    public String getDueDate() { return dueDate; }
    public void setDueDate(String dueDate) { this.dueDate = dueDate; }
    
    public String getStartTime() { return startTime; }
    public void setStartTime(String startTime) { this.startTime = startTime; }
    
    public String getEndTime() { return endTime; }
    public void setEndTime(String endTime) { this.endTime = endTime; }
    
    public String getDateTime() { return dateTime; }
    public void setDateTime(String dateTime) { this.dateTime = dateTime; }
    
    public Activity.PriorityLevel getPriority() { return priority; }
    public void setPriority(Activity.PriorityLevel priority) { this.priority = priority; }
    @JsonSetter("priority")
    public void setPriority(String priority) {
        this.priority = parseEnum(priority, Activity.PriorityLevel.class);
    }
    
    public String getAssignedUser() { return assignedUser; }
    public void setAssignedUser(String assignedUser) { this.assignedUser = assignedUser; }
    
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    
    public String getOrganization() { return organization; }
    public void setOrganization(String organization) { this.organization = organization; }

    public Long getOrganizationId() { return organizationId; }
    public void setOrganizationId(Long organizationId) { this.organizationId = organizationId; }

    public String getOrganizationCategory() { return organizationCategory; }
    public void setOrganizationCategory(String organizationCategory) { this.organizationCategory = organizationCategory; }

    public Long getOrganizationOwnerId() { return organizationOwnerId; }
    public void setOrganizationOwnerId(Long organizationOwnerId) { this.organizationOwnerId = organizationOwnerId; }

    public String getOrganizationOwnerName() { return organizationOwnerName; }
    public void setOrganizationOwnerName(String organizationOwnerName) { this.organizationOwnerName = organizationOwnerName; }

    public Long getAssignedUserId() { return assignedUserId; }
    public void setAssignedUserId(Long assignedUserId) { this.assignedUserId = assignedUserId; }
    
    public Long getPersonId() { return personId; }
    public void setPersonId(Long personId) { this.personId = personId; }
    
    public Long getDealId() { return dealId; }
    public void setDealId(Long dealId) { this.dealId = dealId; }
    
    public String getDealName() { return dealName; }
    public void setDealName(String dealName) { this.dealName = dealName; }
    
    public String getInstagramId() { return instagramId; }
    public void setInstagramId(String instagramId) { this.instagramId = instagramId; }
    
    public String getPhone() { return phone; }
    public void setPhone(String phone) { this.phone = phone; }
    
    public Activity.ActivityStatus getStatus() { return status; }
    public void setStatus(Activity.ActivityStatus status) { this.status = status; }
    @JsonSetter("status")
    public void setStatus(String status) {
        this.status = parseEnum(status, Activity.ActivityStatus.class);
    }
    
    public Boolean getDone() { return done; }
    public void setDone(Boolean done) { this.done = done; }
    
    public String getServiceCategory() { return serviceCategory; }
    public void setServiceCategory(String serviceCategory) { this.serviceCategory = serviceCategory; }
    
    public Integer getDurationMinutes() { return durationMinutes; }
    public void setDurationMinutes(Integer durationMinutes) { this.durationMinutes = durationMinutes; }
    
    public String getAttachmentUrl() { return attachmentUrl; }
    public void setAttachmentUrl(String attachmentUrl) { this.attachmentUrl = attachmentUrl; }

    private <E extends Enum<E>> E parseEnum(String value, Class<E> enumClass) {
        if (value == null || value.isBlank()) {
            return null;
        }
        String normalized = value.trim().toUpperCase().replace(' ', '_');
        try {
            return Enum.valueOf(enumClass, normalized);
        } catch (IllegalArgumentException ex) {
            throw new IllegalArgumentException("Invalid " + enumClass.getSimpleName() + " value: " + value);
        }
    }
}


