package com.brideside.crm.mapper;

import com.brideside.crm.dto.PersonDTO;
import com.brideside.crm.entity.Person;

public class PersonMapper {
    public static PersonDTO toDto(Person p) {
        PersonDTO d = new PersonDTO();
        d.setId(p.getId());
        d.setName(p.getName());
        d.setInstagramId(p.getInstagramId());
        d.setPhone(p.getPhone());
        d.setWeddingDate(p.getWeddingDate());
        d.setVenue(p.getVenue());
        d.setOrganization(p.getOrganization());
        d.setManager(p.getManager());
        d.setCategory(p.getCategory());
        d.setSource(p.getSource());
        d.setCreatedDate(p.getCreatedDate());
        d.setEventType(p.getEventType());
        return d;
    }

    public static void updateEntity(PersonDTO d, Person e) {
        if (d.getName() != null) e.setName(d.getName());
        if (d.getInstagramId() != null) e.setInstagramId(d.getInstagramId());
        if (d.getPhone() != null) e.setPhone(d.getPhone());
        if (d.getWeddingDate() != null) e.setWeddingDate(d.getWeddingDate());
        if (d.getVenue() != null) e.setVenue(d.getVenue());
        if (d.getOrganization() != null) e.setOrganization(d.getOrganization());
        if (d.getManager() != null) e.setManager(d.getManager());
        if (d.getCategory() != null) e.setCategory(d.getCategory());
        if (d.getSource() != null) e.setSource(d.getSource());
        if (d.getCreatedDate() != null) e.setCreatedDate(d.getCreatedDate());
        if (d.getEventType() != null) e.setEventType(d.getEventType());
    }
}


