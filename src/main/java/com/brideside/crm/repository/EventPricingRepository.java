package com.brideside.crm.repository;

import com.brideside.crm.entity.EventPricing;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface EventPricingRepository extends JpaRepository<EventPricing, Long> {

    List<EventPricing> findByVendor_IdOrderBySessionAscArtistLevelAscDisplayOrderAscEventCodeAsc(Long vendorId);

    List<EventPricing> findByVendor_IdAndSessionOrderByDisplayOrderAscEventCodeAsc(Long vendorId, String session);

    void deleteByVendor_Id(Long vendorId);
}
