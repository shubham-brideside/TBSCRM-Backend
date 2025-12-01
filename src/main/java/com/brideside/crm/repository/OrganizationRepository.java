package com.brideside.crm.repository;

import com.brideside.crm.entity.Organization;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface OrganizationRepository extends JpaRepository<Organization, Long> {
    List<Organization> findByOwner_Id(Long ownerId);

    List<Organization> findByOwner_IdIn(List<Long> ownerIds);

    List<Organization> findByGoogleCalendarIdIsNotNull();
}

