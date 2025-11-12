package com.brideside.crm.entity;

import jakarta.persistence.*;
import java.time.*;

@Entity
@Table(name = "activities")
public class Activity {
    
    /**
     * Activity Category Enum
     * Represents the type/category of activity
     */
    public enum ActivityCategory {
        FOLLOW_UP,          // Follow up with lead/customer
        SEND_QUOTES,        // Share proposal/quote
        MEETING,            // Scheduled meeting
        CALL,               // Phone call
        TASK,               // General to-do / activity
        OTHER,              // Fallback/uncategorised
        ACTIVITY,           // Legacy/general activity
        MEETING_SCHEDULER   // Legacy scheduled meeting
    }
    
    /**
     * Activity Status Enum
     * Represents the current status/state of an activity
     */
    public enum ActivityStatus {
        OPEN,           // Activity is open/not started
        PENDING,        // Activity is pending
        IN_PROGRESS,    // Activity is in progress
        COMPLETED,      // Activity is completed
        CANCELLED       // Activity is cancelled
    }
    
    /**
     * Call Type Enum
     * Represents the direction/type of a call
     * Only used when category is CALL
     */
    public enum CallType {
        INBOUND,    // Incoming call
        OUTBOUND,   // Outgoing call
        MISSED      // Missed call
    }
    
    /**
     * Priority Level Enum
     * Represents the priority/urgency of an activity
     */
    public enum PriorityLevel {
        HIGH,
        MEDIUM,
        LOW
    }
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String subject; // e.g., "Follow up", "Call"

    private String type; // optional, mirrors UI type
    private String date; // dd/MM/yyyy (UI friendly) - used as schedule date for calls/meetings
    private String startTime; // HH:mm - used as schedule time for calls/meetings
    private String endTime;   // HH:mm (optional)
    
    @Enumerated(EnumType.STRING)
    @Column(length = 10, columnDefinition = "VARCHAR(10)")
    private PriorityLevel priority;  // HIGH, MEDIUM, LOW
    
    private String assignedUser; // email or name, as used by UI
    @Column(length = 2000)
    private String notes;

    // Temporarily optional while person module integration is finalized
    private Long personId; // Person this activity belongs to

    private String organization; // redundant for quick filtering in list view
    private boolean done = false;

    // New fields to support Tasks/Calls/Meetings columns
    @Enumerated(EnumType.STRING)
    @Column(length = 20, columnDefinition = "VARCHAR(20)")
    private ActivityCategory category; // ACTIVITY, CALL, MEETING_SCHEDULER
    
    private String dealName;
    private String instagramId;
    private String phone;
    private String dueDate; // for Tasks (dd/MM/yyyy)
    private String scheduleBy; // user who scheduled call/meeting
    
    @Enumerated(EnumType.STRING)
    @Column(length = 20, columnDefinition = "VARCHAR(20)")
    private ActivityStatus status; // OPEN, PENDING, IN_PROGRESS, COMPLETED, CANCELLED
    
    @Enumerated(EnumType.STRING)
    @Column(length = 10, columnDefinition = "VARCHAR(10)")
    private CallType callType; // INBOUND, OUTBOUND, MISSED (for Calls only)

    // Deal linkage (each activity belongs to exactly one deal)
    // Temporarily optional while deals module integration is finalized
    private Long dealId; // FK to deals.id (not enforced here)

    // Canonical timestamp (optional for testing; can be null)
    private String dateTime; // ISO string (yyyy-MM-dd'T'HH:mm:ss) or agreed format

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
    public PriorityLevel getPriority() { return priority; }
    public void setPriority(PriorityLevel priority) { this.priority = priority; }
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
    public ActivityCategory getCategory() { return category; }
    public void setCategory(ActivityCategory category) { this.category = category; }
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
    public ActivityStatus getStatus() { return status; }
    public void setStatus(ActivityStatus status) { this.status = status; }
    public CallType getCallType() { return callType; }
    public void setCallType(CallType callType) { this.callType = callType; }
    public Long getDealId() { return dealId; }
    public void setDealId(Long dealId) { this.dealId = dealId; }
    public String getDateTime() { return dateTime; }
    public void setDateTime(String dateTime) { this.dateTime = dateTime; }
    public Instant getCreatedAt() { return createdAt; }
    public void setCreatedAt(Instant createdAt) { this.createdAt = createdAt; }
    public Instant getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(Instant updatedAt) { this.updatedAt = updatedAt; }
}


