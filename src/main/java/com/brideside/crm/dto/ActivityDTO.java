package com.brideside.crm.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

public class ActivityDTO {
    private Long id;

    @NotBlank
    private String subject;
    private String type;
    private String date;       // schedule date for calls/meetings
    private String startTime;  // schedule time for calls/meetings
    private String endTime;
    private String priority;
    private String assignedUser;
    private String notes;

    @NotNull
    private Long personId;
    private String organization;
    private Boolean done;

    // Category-specific columns
    private String category; // Activity, Call, Meeting scheduler
    private String dealName;
    private String instagramId;
    private String phone;
    private String dueDate;   // for Tasks
    private String scheduleBy; // for Calls/Meetings
    private String status;    // for Tasks or general status
    private String callType;  // for Calls only

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
    public String getPriority() { return priority; }
    public void setPriority(String priority) { this.priority = priority; }
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
    public String getCategory() { return category; }
    public void setCategory(String category) { this.category = category; }
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
    public String getStatus() { return status; }
    public void setStatus(String status) { this.status = status; }
    public String getCallType() { return callType; }
    public void setCallType(String callType) { this.callType = callType; }
}


