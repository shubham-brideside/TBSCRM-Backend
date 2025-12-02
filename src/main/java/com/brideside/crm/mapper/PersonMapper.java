package com.brideside.crm.mapper;

import com.brideside.crm.dto.PersonDTO;
import com.brideside.crm.entity.Category;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Person;
import com.brideside.crm.entity.User;

public final class PersonMapper {

    private PersonMapper() {
    }

    public static PersonDTO toDto(Person person) {
        PersonDTO dto = new PersonDTO();
        dto.setId(person.getId());
        dto.setName(person.getName());
        dto.setInstagramId(person.getInstagramId());
        dto.setPhone(person.getPhone());
        dto.setEmail(person.getEmail());
        dto.setLeadDate(person.getLeadDate());
        dto.setLabel(person.getLabel());
        dto.setSource(person.getSource());
        dto.setSubSource(person.getSubSource());
        dto.setCreatedAt(person.getCreatedAt());
        dto.setUpdatedAt(person.getUpdatedAt());

        Organization organization = person.getOrganization();
        if (organization != null) {
            dto.setOrganizationId(organization.getId());
            dto.setOrganizationName(organization.getName());
        }

        User owner = person.getOwner();
        if (owner != null) {
            dto.setOwnerId(owner.getId());
            StringBuilder nameBuilder = new StringBuilder();
            if (owner.getFirstName() != null) {
                nameBuilder.append(owner.getFirstName());
            }
            if (owner.getLastName() != null && !owner.getLastName().isBlank()) {
                if (!nameBuilder.isEmpty()) {
                    nameBuilder.append(" ");
                }
                nameBuilder.append(owner.getLastName());
            }
            dto.setOwnerDisplayName(nameBuilder.isEmpty() ? owner.getEmail() : nameBuilder.toString());
            dto.setOwnerEmail(owner.getEmail());
        }

        Category category = person.getCategory();
        if (category != null) {
            dto.setCategoryId(category.getId());
            dto.setCategoryName(category.getName());
        }

        return dto;
    }

    public static void updateEntity(PersonDTO dto, Person entity) {
        if (dto.getName() != null) {
            entity.setName(dto.getName());
        }
        if (dto.getInstagramId() != null) {
            entity.setInstagramId(dto.getInstagramId());
        }
        if (dto.getPhone() != null) {
            entity.setPhone(dto.getPhone());
        }
        if (dto.getEmail() != null) {
            entity.setEmail(dto.getEmail());
        }
        if (dto.getLeadDate() != null) {
            entity.setLeadDate(dto.getLeadDate());
        }
        if (dto.getLabel() != null) {
            entity.setLabel(dto.getLabel());
        }
        if (dto.getSource() != null) {
            entity.setSource(dto.getSource());
        }
        if (dto.getSubSource() != null) {
            entity.setSubSource(dto.getSubSource());
        }
    }
}

