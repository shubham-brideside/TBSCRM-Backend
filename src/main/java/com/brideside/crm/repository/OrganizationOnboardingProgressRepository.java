package com.brideside.crm.repository;

import com.brideside.crm.entity.OrganizationOnboardingProgress;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface OrganizationOnboardingProgressRepository extends JpaRepository<OrganizationOnboardingProgress, Long> {

    Optional<OrganizationOnboardingProgress> findByOrganization_Id(Long organizationId);

    void deleteByOrganization_Id(Long organizationId);
}
