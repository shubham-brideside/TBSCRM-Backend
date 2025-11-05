package com.brideside.crm.entity;

import jakarta.persistence.*;
import java.time.*;

@Entity
@Table(name = "activities")
public class Activity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String subject; // e.g., "Follow up", "Call"

    private String type; // optional, mirrors UI type
    private String date; // dd/MM/yyyy (UI friendly) - used as schedule date for calls/meetings
    private String startTime; // HH:mm - used as schedule time for calls/meetings
    private String endTime;   // HH:mm (optional)
    private String priority;  // e.g., Low/Normal/High
    private String assignedUser; // email or name, as used by UI
    @Column(length = 2000)
    private String notes;

    @Column(nullable = false)
    private Long personId; // Person this activity belongs to

    private String organization; // redundant for quick filtering in list view
    private boolean done = false;

    // New fields to support Tasks/Calls/Meetings columns
    private String category; // Activity, Call, Meeting scheduler
    private String dealName;
    private String instagramId;
    private String phone;
    private String dueDate; // for Tasks (dd/MM/yyyy)
    private String scheduleBy; // user who scheduled call/meeting
    private String status; // task status (e.g., Open/Closed) or general status
    private String callType; // for Calls only (e.g., Incoming/Outgoing)

    private Instant createdAt = Instant.now();
    private Instant updatedAt = Instant.now();

    @PreUpdate
    public void preUpdate() { this.updatedAt = Instant.now(); }

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
    public boolean isDone() { return done; }
    public void setDone(boolean done) { this.done = done; }
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
    public Instant getCreatedAt() { return createdAt; }
    public void setCreatedAt(Instant createdAt) { this.createdAt = createdAt; }
    public Instant getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Instant updatedAt) { this.updatedAt = updatedAt; }
}


