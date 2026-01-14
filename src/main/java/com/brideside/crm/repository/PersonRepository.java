package com.brideside.crm.repository;

import com.brideside.crm.entity.Person;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface PersonRepository extends JpaRepository<Person, Long>, JpaSpecificationExecutor<Person> {
    List<Person> findByOwner_Id(Long ownerId);
    
    // Methods that exclude soft-deleted persons
    List<Person> findByIsDeletedFalse();

    // All persons (including soft-deleted) linked to a given organization
    List<Person> findByOrganization_Id(Long organizationId);

    // Bulk detach organization from persons to avoid per-row updates and lock issues
    @Modifying
    @Query("UPDATE Person p SET p.organization = null WHERE p.organization.id = :organizationId")
    int clearOrganizationByOrganizationId(@Param("organizationId") Long organizationId);
}

