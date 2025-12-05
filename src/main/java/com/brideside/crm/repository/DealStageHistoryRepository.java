package com.brideside.crm.repository;

import com.brideside.crm.entity.DealStageHistory;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface DealStageHistoryRepository extends JpaRepository<DealStageHistory, Long> {

    // Find current stage history for a deal
    Optional<DealStageHistory> findByDealIdAndIsCurrentTrue(Long dealId);

    // Find all stage history for a deal, ordered by entered_at
    List<DealStageHistory> findByDealIdOrderByEnteredAtAsc(Long dealId);

    // Find stage history for a specific deal and stage
    List<DealStageHistory> findByDealIdAndStageIdOrderByEnteredAtAsc(Long dealId, Long stageId);

    // Find all current stages (for deals currently in a stage)
    List<DealStageHistory> findByIsCurrentTrue();
}

