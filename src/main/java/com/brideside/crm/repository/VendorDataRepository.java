package com.brideside.crm.repository;

import com.brideside.crm.entity.VendorData;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface VendorDataRepository extends JpaRepository<VendorData, Long> {

    Optional<VendorData> findByVendor_Id(Long vendorId);

    boolean existsByVendor_Id(Long vendorId);
}
