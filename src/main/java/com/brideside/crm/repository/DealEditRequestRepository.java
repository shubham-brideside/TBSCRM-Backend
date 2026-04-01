package com.brideside.crm.repository;

import com.brideside.crm.entity.DealEditRequest;
import com.brideside.crm.entity.DealEditRequestStatus;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface DealEditRequestRepository extends JpaRepository<DealEditRequest, Long> {

    List<DealEditRequest> findByStatusOrderByCreatedAtDesc(DealEditRequestStatus status);

    List<DealEditRequest> findByDealIdAndStatus(Long dealId, DealEditRequestStatus status);
}

