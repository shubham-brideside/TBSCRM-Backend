package com.brideside.crm.dto;

import com.brideside.crm.entity.Activity;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;

@Schema(description = "Activity data transfer object")
public class ActivityDTO {
    @Schema(description = "Activity ID (auto-generated, omit for POST requests)", type = "integer", format = "int64", accessMode = Schema.AccessMode.READ_ONLY)
    private Long id;

    @NotBlank
    @Schema(description = "Activity subject/title", required = true)
    private String subject;
    
    @Schema(description = "Activity type")
    private String type;
    
    @Schema(description = "Schedule date for calls/meetings (format: dd/MM/yyyy)")
    private String date;       // schedule date for calls/meetings
    
    @Schema(description = "Start time (format: HH:mm)")
    private String startTime;  // schedule time for calls/meetings
    
    @Schema(description = "End time (format: HH:mm)")
    private String endTime;
    
    @Schema(description = "Priority level of the activity", allowableValues = {"HIGH", "MEDIUM", "LOW"})
    private Activity.PriorityLevel priority;
    
    @Schema(description = "Assigned user email")
    private String assignedUser;
    
    @Schema(description = "Activity notes")
    private String notes;

    // Temporarily optional for testing without linking
    @Schema(description = "Person ID (optional)", type = "integer", format = "int64")
    private Long personId;
    
    @Schema(description = "Organization name")
    private String organization;
    
    @Schema(description = "Done status", defaultValue = "false", type = "boolean")
    private Boolean done = false;

    // Category-specific columns
    @Schema(description = "Category of the activity", allowableValues = {"FOLLOW_UP", "SEND_QUOTES", "MEETING", "CALL", "TASK", "OTHER", "ACTIVITY", "MEETING_SCHEDULER"})
    private Activity.ActivityCategory category;
    
    @Schema(description = "Deal name")
    private String dealName;
    
    @Schema(description = "Instagram ID")
    private String instagramId;
    
    @Schema(description = "Phone number")
    private String phone;
    
    @Schema(description = "Due date for tasks (format: dd/MM/yyyy)")
    private String dueDate;   // for Tasks
    
    @Schema(description = "Scheduled by user email")
    private String scheduleBy; // for Calls/Meetings
    
    @Schema(description = "Status of the activity", allowableValues = {"OPEN", "PENDING", "IN_PROGRESS", "COMPLETED", "CANCELLED"})
    private Activity.ActivityStatus status;    // OPEN, PENDING, IN_PROGRESS, COMPLETED, CANCELLED
    
    @Schema(description = "Type of call (only for CALL category)", allowableValues = {"INBOUND", "OUTBOUND", "MISSED"})
    private Activity.CallType callType;  // INBOUND, OUTBOUND, MISSED (for Calls only)

    // Temporarily optional for testing without linking
    @Schema(description = "Deal ID (optional)", type = "integer", format = "int64")
    private Long dealId;      // each activity belongs to one deal

    @Schema(description = "Optional canonical timestamp for the activity")
    private String dateTime;  // optional canonical timestamp for the activity

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    public String getSubject() { return subject; }
    public void setSubject(String subject) { this.subject = subject; }
    public String getType() { return type; }
    public void setType(String type) { this.type = type; }
    public String getDate() { return date; }
    public void setDate(String date) { this.date = date; }
    public String getStartTime() { return startTime; }
    public void setStartTime(String startTime) { this.startTime = startTime; }
    public String getEndTime() { return endTime; }
    public void setEndTime(String endTime) { this.endTime = endTime; }
    public Activity.PriorityLevel getPriority() { return priority; }
    public void setPriority(Activity.PriorityLevel priority) { this.priority = priority; }
    public String getAssignedUser() { return assignedUser; }
    public void setAssignedUser(String assignedUser) { this.assignedUser = assignedUser; }
    public String getNotes() { return notes; }
    public void setNotes(String notes) { this.notes = notes; }
    public Long getPersonId() { return personId; }
    public void setPersonId(Long personId) { this.personId = personId; }
    public String getOrganization() { return organization; }
    public void setOrganization(String organization) { this.organization = organization; }
    public Boolean getDone() { return done; }
    public void setDone(Boolean done) { this.done = done; }
    public Activity.ActivityCategory getCategory() { return category; }
    public void setCategory(Activity.ActivityCategory category) { this.category = category; }
    public String getDealName() { return dealName; }
    public void setDealName(String dealName) { this.dealName = dealName; }
    public String getInstagramId() { return instagramId; }
    public void setInstagramId(String instagramId) { this.instagramId = instagramId; }
    public String getPhone() { return phone; }
    public void setPhone(String phone) { this.phone = phone; }
    public String getDueDate() { return dueDate; }
    public void setDueDate(String dueDate) { this.dueDate = dueDate; }
    public String getScheduleBy() { return scheduleBy; }
    public void setScheduleBy(String scheduleBy) { this.scheduleBy = scheduleBy; }
    public Activity.ActivityStatus getStatus() { return status; }
    public void setStatus(Activity.ActivityStatus status) { this.status = status; }
    public Activity.CallType getCallType() { return callType; }
    public void setCallType(Activity.CallType callType) { this.callType = callType; }
    public Long getDealId() { return dealId; }
    public void setDealId(Long dealId) { this.dealId = dealId; }
    public String getDateTime() { return dateTime; }
    public void setDateTime(String dateTime) { this.dateTime = dateTime; }
}


