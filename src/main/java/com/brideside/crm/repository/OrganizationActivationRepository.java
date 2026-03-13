package com.brideside.crm.repository;

import com.brideside.crm.entity.OrganizationActivation;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface OrganizationActivationRepository extends JpaRepository<OrganizationActivation, Long> {

    Optional<OrganizationActivation> findByOrganization_Id(Long organizationId);

    boolean existsByOrganization_Id(Long organizationId);
}

