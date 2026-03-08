package com.brideside.crm.repository;

import com.brideside.crm.entity.VendorTeamMember;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface VendorTeamMemberRepository extends JpaRepository<VendorTeamMember, Long> {

    List<VendorTeamMember> findByVendor_IdOrderByIdAsc(Long vendorId);

    Optional<VendorTeamMember> findByIdAndVendor_Id(Long id, Long vendorId);
}
