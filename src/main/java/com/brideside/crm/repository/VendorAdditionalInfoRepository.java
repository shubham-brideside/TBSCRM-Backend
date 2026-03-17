package com.brideside.crm.repository;

import com.brideside.crm.entity.VendorAdditionalInfo;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface VendorAdditionalInfoRepository extends JpaRepository<VendorAdditionalInfo, Long> {

    Optional<VendorAdditionalInfo> findByVendor_Id(Long vendorId);

    boolean existsByVendor_Id(Long vendorId);
}

