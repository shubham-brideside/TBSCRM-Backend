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
        d.setOrganizationId(e.getOrganizationId());
        if (e.getOrganizationRef() != null) {
            if (e.getOrganizationRef().getCategory() != null) {
                d.setOrganizationCategory(e.getOrganizationRef().getCategory().name());
            }
            if (e.getOrganizationRef().getOwner() != null) {
                d.setOrganizationOwnerId(e.getOrganizationRef().getOwner().getId());
                String ownerFirst = e.getOrganizationRef().getOwner().getFirstName();
                String ownerLast = e.getOrganizationRef().getOwner().getLastName();
                String fullName = ((ownerFirst != null ? ownerFirst : "").trim() + " " +
                        (ownerLast != null ? ownerLast : "").trim()).trim();
                d.setOrganizationOwnerName(fullName.isEmpty() ? null : fullName);
            }
        }
        d.setAssignedUserId(e.getAssignedUserId());
        d.setDone(e.isDone());
        d.setCategory(e.getCategory());
        // Use deal name from Deal entity if available, otherwise fall back to dealName field
        if (e.getDealRef() != null && e.getDealRef().getName() != null) {
            d.setDealName(e.getDealRef().getName());
        } else {
            d.setDealName(e.getDealName());
        }
        d.setInstagramId(e.getInstagramId());
        d.setPhone(e.getPhone());
        d.setDueDate(e.getDueDate());
        d.setStatus(e.getStatus());
        d.setDealId(e.getDealId());
        d.setDateTime(e.getDateTime());
        d.setServiceCategory(e.getServiceCategory());
        d.setDurationMinutes(e.getDurationMinutes());
        d.setAttachmentUrl(e.getAttachmentUrl());
        return d;
    }

    /**
     * Update entity from DTO for create operations.
     * Excludes: done (managed via toggle endpoint). Legacy scheduleBy/callType fields are no longer exposed.
     */
    public static void updateEntityForCreate(ActivityDTO d, Activity e) {
        if (d.getSubject() != null) e.setSubject(d.getSubject());
        if (d.getType() != null) e.setType(d.getType());
        if (d.getDate() != null) e.setDate(d.getDate());
        if (d.getStartTime() != null) e.setStartTime(d.getStartTime());
        if (d.getEndTime() != null) e.setEndTime(d.getEndTime());
        if (d.getPriority() != null) e.setPriority(d.getPriority());
        if (d.getAssignedUser() != null) e.setAssignedUser(d.getAssignedUser());
        if (d.getNotes() != null) e.setNotes(d.getNotes());
        if (d.getPersonId() != null) e.setPersonId(d.getPersonId());
        if (d.getOrganization() != null) e.setOrganization(d.getOrganization());
        if (d.getOrganizationId() != null) e.setOrganizationId(d.getOrganizationId());
        if (d.getAssignedUserId() != null) e.setAssignedUserId(d.getAssignedUserId());
        // done is excluded - managed via POST /api/activities/{id}/done
        if (d.getCategory() != null) e.setCategory(d.getCategory());
        if (d.getDealName() != null) e.setDealName(d.getDealName());
        if (d.getInstagramId() != null) e.setInstagramId(d.getInstagramId());
        if (d.getPhone() != null) e.setPhone(d.getPhone());
        if (d.getDueDate() != null) e.setDueDate(d.getDueDate());
        if (d.getStatus() != null) e.setStatus(d.getStatus());
        if (d.getDealId() != null) e.setDealId(d.getDealId());
        if (d.getDateTime() != null) e.setDateTime(d.getDateTime());
        if (d.getServiceCategory() != null) e.setServiceCategory(d.getServiceCategory());
        if (d.getDurationMinutes() != null) e.setDurationMinutes(d.getDurationMinutes());
        if (d.getAttachmentUrl() != null) e.setAttachmentUrl(d.getAttachmentUrl());
    }

    /**
     * Update entity from DTO for update operations.
     * Includes all fields (for partial updates).
     */
    public static void updateEntity(ActivityDTO d, Activity e) {
        if (d.getSubject() != null) e.setSubject(d.getSubject());
        if (d.getType() != null) e.setType(d.getType());
        if (d.getDate() != null) e.setDate(d.getDate());
        if (d.getStartTime() != null) e.setStartTime(d.getStartTime());
        if (d.getEndTime() != null) e.setEndTime(d.getEndTime());
        if (d.getPriority() != null) e.setPriority(d.getPriority());
        if (d.getAssignedUser() != null) e.setAssignedUser(d.getAssignedUser());
        if (d.getNotes() != null) e.setNotes(d.getNotes());
        if (d.getPersonId() != null) e.setPersonId(d.getPersonId());
        if (d.getOrganization() != null) e.setOrganization(d.getOrganization());
        if (d.getOrganizationId() != null) e.setOrganizationId(d.getOrganizationId());
        if (d.getDone() != null) e.setDone(d.getDone());
        if (d.getCategory() != null) e.setCategory(d.getCategory());
        if (d.getDealName() != null) e.setDealName(d.getDealName());
        if (d.getInstagramId() != null) e.setInstagramId(d.getInstagramId());
        if (d.getPhone() != null) e.setPhone(d.getPhone());
        if (d.getDueDate() != null) e.setDueDate(d.getDueDate());
        if (d.getStatus() != null) e.setStatus(d.getStatus());
        if (d.getDealId() != null) e.setDealId(d.getDealId());
        if (d.getDateTime() != null) e.setDateTime(d.getDateTime());
        if (d.getServiceCategory() != null) e.setServiceCategory(d.getServiceCategory());
        if (d.getDurationMinutes() != null) e.setDurationMinutes(d.getDurationMinutes());
        if (d.getAttachmentUrl() != null) e.setAttachmentUrl(d.getAttachmentUrl());
        if (d.getAssignedUserId() != null) e.setAssignedUserId(d.getAssignedUserId());
    }
}


