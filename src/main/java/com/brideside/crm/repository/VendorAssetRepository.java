package com.brideside.crm.repository;

import com.brideside.crm.entity.VendorAsset;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface VendorAssetRepository extends JpaRepository<VendorAsset, Long> {

    List<VendorAsset> findByVendor_Id(Long vendorId);

    List<VendorAsset> findByOrganization_Id(Long organizationId);

    void deleteByOrganization_Id(Long organizationId);

    Optional<VendorAsset> findByIdAndVendor_Id(Long id, Long vendorId);

    Optional<VendorAsset> findByVendor_IdAndOrganization_Id(Long vendorId, Long organizationId);
}
