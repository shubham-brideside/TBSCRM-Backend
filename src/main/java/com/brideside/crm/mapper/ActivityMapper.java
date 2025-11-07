package com.brideside.crm.mapper;

import com.brideside.crm.dto.ActivityDTO;
import com.brideside.crm.entity.Activity;

public class ActivityMapper {
    public static ActivityDTO toDto(Activity e) {
        ActivityDTO d = new ActivityDTO();
        d.setId(e.getId());
        d.setSubject(e.getSubject());
        d.setType(e.getType());
        d.setDate(e.getDate());
        d.setStartTime(e.getStartTime());
        d.setEndTime(e.getEndTime());
        d.setPriority(e.getPriority());
        d.setAssignedUser(e.getAssignedUser());
        d.setNotes(e.getNotes());
        d.setPersonId(e.getPersonId());
        d.setOrganization(e.getOrganization());
        d.setDone(e.isDone());
        d.setCategory(e.getCategory());
        // Map dealName from entity to both deal and dealName in DTO for frontend compatibility
        d.setDealName(e.getDealName());
        d.setDeal(e.getDealName()); // Frontend expects 'deal' field
        d.setInstagramId(e.getInstagramId());
        d.setPhone(e.getPhone());
        d.setDueDate(e.getDueDate());
        d.setScheduleBy(e.getScheduleBy());
        d.setStatus(e.getStatus());
        d.setCallType(e.getCallType());
        d.setDealId(e.getDealId());
        d.setDateTime(e.getDateTime());
        return d;
    }

    public static void updateEntity(ActivityDTO d, Activity e) {
        if (d.getSubject() != null) e.setSubject(d.getSubject());
        if (d.getType() != null) e.setType(d.getType());
        if (d.getDate() != null) e.setDate(d.getDate());
        if (d.getStartTime() != null) e.setStartTime(d.getStartTime());
        if (d.getEndTime() != null) e.setEndTime(d.getEndTime());
        if (d.getPriority() != null) e.setPriority(d.getPriority());
        if (d.getAssignedUser() != null) e.setAssignedUser(d.getAssignedUser());
        if (d.getNotes() != null) e.setNotes(d.getNotes());
        // personId and dealId are optional - set to null if not provided
        // Frontend doesn't need to send these fields, they'll be null by default
        // Will be used when person/deal pages are integrated
        e.setPersonId(d.getPersonId()); // Can be null
        e.setDealId(d.getDealId()); // Can be null, defaults to null
        if (d.getOrganization() != null) e.setOrganization(d.getOrganization());
        if (d.getDone() != null) e.setDone(d.getDone());
        if (d.getCategory() != null) e.setCategory(d.getCategory());
        // Handle both 'deal' and 'dealName' - prefer 'deal' if both are provided
        // Frontend sends 'deal' field, but we store it as 'dealName' in entity
        if (d.getDeal() != null) {
            e.setDealName(d.getDeal());
        } else if (d.getDealName() != null) {
            e.setDealName(d.getDealName());
        }
        if (d.getInstagramId() != null) e.setInstagramId(d.getInstagramId());
        if (d.getPhone() != null) e.setPhone(d.getPhone());
        if (d.getDueDate() != null) e.setDueDate(d.getDueDate());
        if (d.getScheduleBy() != null) e.setScheduleBy(d.getScheduleBy());
        if (d.getStatus() != null) e.setStatus(d.getStatus());
        if (d.getCallType() != null) e.setCallType(d.getCallType());
        if (d.getDateTime() != null) e.setDateTime(d.getDateTime());
    }
}


