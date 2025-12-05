package com.brideside.crm.service;

import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.DealStageHistory;
import com.brideside.crm.repository.DealRepository;
import com.brideside.crm.repository.DealStageHistoryRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Service to migrate existing deals to have stage history records.
 * This should be run once after deploying the stage history feature.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DealStageHistoryMigrationService {

    private final DealRepository dealRepository;
    private final DealStageHistoryRepository historyRepository;

    /**
     * Migrate all existing deals to have stage history entries.
     * Creates an initial history entry for each deal's current stage using deal.createdAt as entered_at.
     */
    @Transactional
    public void migrateExistingDeals() {
        log.info("Starting migration of existing deals to stage history...");
        
        List<Deal> deals = dealRepository.findAll();
        int migratedCount = 0;
        int skippedCount = 0;

        for (Deal deal : deals) {
            // Skip soft-deleted deals
            if (Boolean.TRUE.equals(deal.getIsDeleted())) {
                skippedCount++;
                continue;
            }

            // Check if deal already has stage history
            if (historyRepository.findByDealIdAndIsCurrentTrue(deal.getId()).isPresent()) {
                log.debug("Deal {} already has stage history, skipping", deal.getId());
                skippedCount++;
                continue;
            }

            // Only create history if deal has a stage
            if (deal.getStage() != null) {
                DealStageHistory history = new DealStageHistory();
                history.setDeal(deal);
                history.setStage(deal.getStage());
                // Use deal.createdAt as entered_at, or current time if createdAt is null
                history.setEnteredAt(deal.getCreatedAt() != null ? 
                    deal.getCreatedAt() : LocalDateTime.now());
                history.setIsCurrent(true);
                historyRepository.save(history);
                
                migratedCount++;
                log.debug("Migrated deal {} to stage history (stage: {})", 
                    deal.getId(), deal.getStage().getId());
            } else {
                skippedCount++;
                log.debug("Deal {} has no stage, skipping", deal.getId());
            }
        }

        log.info("Migration completed. Migrated: {}, Skipped: {}, Total: {}", 
            migratedCount, skippedCount, deals.size());
    }
}

