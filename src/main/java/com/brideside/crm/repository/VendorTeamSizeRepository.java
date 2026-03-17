package com.brideside.crm.repository;

import com.brideside.crm.entity.VendorTeamSize;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface VendorTeamSizeRepository extends JpaRepository<VendorTeamSize, Long> {

    List<VendorTeamSize> findByVendor_IdOrderByIdAsc(Long vendorId);

    void deleteByVendor_Id(Long vendorId);
}

