package com.brideside.crm.service;

import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.DealStageHistory;
import com.brideside.crm.entity.Stage;
import com.brideside.crm.repository.DealStageHistoryRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Service
@RequiredArgsConstructor
@Slf4j
public class DealStageHistoryService {

    private final DealStageHistoryRepository historyRepository;

    /**
     * Record when a deal enters a new stage
     * This should be called whenever a deal's stage is updated
     */
    @Transactional
    public void recordStageEntry(Deal deal, Stage newStage) {
        // Mark previous current stage as exited
        Optional<DealStageHistory> previousCurrent =
            historyRepository.findByDealIdAndIsCurrentTrue(deal.getId());

        if (previousCurrent.isPresent()) {
            DealStageHistory previous = previousCurrent.get();
            LocalDateTime now = LocalDateTime.now();
            previous.setExitedAt(now);
            previous.setIsCurrent(false);

            // Calculate days in stage
            if (previous.getEnteredAt() != null) {
                long days = ChronoUnit.DAYS.between(previous.getEnteredAt(), now);
                previous.setDaysInStage((int) days);
            }

            historyRepository.save(previous);
        }

        // Create new entry for current stage
        DealStageHistory newEntry = new DealStageHistory();
        newEntry.setDeal(deal);
        newEntry.setStage(newStage);
        newEntry.setEnteredAt(LocalDateTime.now());
        newEntry.setIsCurrent(true);

        historyRepository.save(newEntry);

        log.info("Recorded stage entry: Deal {} entered stage {} at {}",
            deal.getId(), newStage.getId(), newEntry.getEnteredAt());
    }

    /**
     * Get days a deal has been in a specific stage
     * Returns 0 if deal has never been in that stage
     */
    public int getDaysInStage(Long dealId, Long stageId) {
        List<DealStageHistory> histories =
            historyRepository.findByDealIdAndStageIdOrderByEnteredAtAsc(dealId, stageId);

        if (histories.isEmpty()) {
            return 0; // Deal has never been in this stage
        }

        // Check if currently in this stage
        Optional<DealStageHistory> current = histories.stream()
            .filter(h -> Boolean.TRUE.equals(h.getIsCurrent()))
            .findFirst();

        if (current.isPresent()) {
            // Calculate days from entry to now
            DealStageHistory currentHistory = current.get();
            LocalDateTime enteredAt = currentHistory.getEnteredAt();
            LocalDateTime now = LocalDateTime.now();
            return (int) ChronoUnit.DAYS.between(enteredAt, now);
        }

        // Deal was in this stage before, return the last recorded days
        DealStageHistory lastHistory = histories.get(histories.size() - 1);
        return lastHistory.getDaysInStage() != null ? lastHistory.getDaysInStage() : 0;
    }

    /**
     * Get days a deal has been in its current stage
     */
    public int getDaysInCurrentStage(Long dealId) {
        Optional<DealStageHistory> current =
            historyRepository.findByDealIdAndIsCurrentTrue(dealId);

        if (current.isEmpty()) {
            return 0;
        }

        DealStageHistory currentHistory = current.get();
        LocalDateTime enteredAt = currentHistory.getEnteredAt();
        LocalDateTime now = LocalDateTime.now();
        return (int) ChronoUnit.DAYS.between(enteredAt, now);
    }

    /**
     * Get all stage durations for a deal
     * Returns a map of stageId -> days
     */
    public Map<Long, Integer> getAllStageDurations(Long dealId) {
        List<DealStageHistory> histories =
            historyRepository.findByDealIdOrderByEnteredAtAsc(dealId);

        Map<Long, Integer> durations = new HashMap<>();

        for (DealStageHistory history : histories) {
            Long stageId = history.getStage().getId();

            if (Boolean.TRUE.equals(history.getIsCurrent())) {
                // Calculate current days
                LocalDateTime enteredAt = history.getEnteredAt();
                LocalDateTime now = LocalDateTime.now();
                int days = (int) ChronoUnit.DAYS.between(enteredAt, now);
                durations.put(stageId, days);
            } else if (history.getDaysInStage() != null) {
                // Use recorded days
                durations.put(stageId, history.getDaysInStage());
            } else {
                // Calculate if not recorded
                if (history.getExitedAt() != null) {
                    long days = ChronoUnit.DAYS.between(history.getEnteredAt(), history.getExitedAt());
                    durations.put(stageId, (int) days);
                }
            }
        }

        return durations;
    }
}

