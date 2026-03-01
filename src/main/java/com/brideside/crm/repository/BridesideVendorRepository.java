package com.brideside.crm.repository;

import com.brideside.crm.entity.BridesideVendor;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface BridesideVendorRepository extends JpaRepository<BridesideVendor, Long> {

    List<BridesideVendor> findByOrganization_Id(Long organizationId);

    Optional<BridesideVendor> findByIdAndOrganization_Id(Long id, Long organizationId);

    boolean existsByUsername(String username);

    Optional<BridesideVendor> findByUsername(String username);
}

