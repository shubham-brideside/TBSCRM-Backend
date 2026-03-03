package com.brideside.crm.service;

import com.brideside.crm.dto.DealDtos;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.DealStageHistory;
import com.brideside.crm.entity.Stage;
import com.brideside.crm.repository.DealStageHistoryRepository;
import com.brideside.crm.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

@Service
@RequiredArgsConstructor
@Slf4j
public class DealStageHistoryService {

    private final DealStageHistoryRepository historyRepository;
    private final UserRepository userRepository;

    /**
     * Record when a deal enters a new stage
     * This should be called whenever a deal's stage is updated
     */
    @Transactional
    public void recordStageEntry(Deal deal, Stage newStage) {
        // Mark previous current stage as exited
        List<DealStageHistory> currentEntries =
            historyRepository.findByDealIdAndIsCurrentTrue(deal.getId());

        if (!currentEntries.isEmpty()) {
            LocalDateTime now = LocalDateTime.now();

            // Sort to have deterministic processing order (oldest first)
            currentEntries.sort(Comparator
                .comparing(DealStageHistory::getEnteredAt, Comparator.nullsFirst(Comparator.naturalOrder()))
                .thenComparing(DealStageHistory::getId, Comparator.nullsFirst(Comparator.naturalOrder())));

            for (DealStageHistory previous : currentEntries) {
                previous.setExitedAt(now);
                previous.setIsCurrent(false);

                if (previous.getEnteredAt() != null) {
                    long days = ChronoUnit.DAYS.between(previous.getEnteredAt(), now);
                    previous.setDaysInStage((int) days);
                }

                historyRepository.save(previous);
            }
        }

        // Create new entry for current stage
        DealStageHistory newEntry = new DealStageHistory();
        newEntry.setDeal(deal);
        newEntry.setStage(newStage);
        newEntry.setEnteredAt(LocalDateTime.now());
        newEntry.setIsCurrent(true);
        populateMovedBy(newEntry);

        historyRepository.save(newEntry);

        log.info("Recorded stage entry: Deal {} entered stage {} at {}",
            deal.getId(), newStage.getId(), newEntry.getEnteredAt());
    }

    private void populateMovedBy(DealStageHistory entry) {
        try {
            Authentication auth = SecurityContextHolder.getContext() != null
                    ? SecurityContextHolder.getContext().getAuthentication()
                    : null;
            if (auth == null || !auth.isAuthenticated()) return;

            Object principal = auth.getPrincipal();
            String email = null;
            if (principal instanceof UserDetails ud) {
                email = ud.getUsername();
            } else if (principal instanceof String s) {
                email = s;
            }

            if (email == null || email.isBlank() || "anonymousUser".equalsIgnoreCase(email)) return;

            userRepository.findByEmail(email).ifPresent(u -> {
                entry.setMovedByUserId(u.getId());
                String fullName = ((u.getFirstName() != null ? u.getFirstName() : "").trim()
                        + " "
                        + (u.getLastName() != null ? u.getLastName() : "").trim()).trim();
                entry.setMovedByName(!fullName.isEmpty() ? fullName : u.getEmail());
            });
        } catch (Exception ignored) {
            // Best-effort only; stage history should never fail due to missing auth context.
        }
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
        List<DealStageHistory> currentEntries =
            historyRepository.findByDealIdAndIsCurrentTrue(dealId);

        if (currentEntries.isEmpty()) {
            return 0;
        }

        DealStageHistory currentHistory = currentEntries.stream()
            .filter(h -> h.getEnteredAt() != null)
            .max(Comparator.comparing(DealStageHistory::getEnteredAt)
                .thenComparing(DealStageHistory::getId, Comparator.nullsFirst(Comparator.naturalOrder())))
            .orElse(currentEntries.get(0));

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

    /**
     * Get average deal timeline: average days a deal spends in each stage (by stage name).
     * Uses only completed stage visits (deals that have left the stage).
     */
    public DealDtos.AverageDealTimelineResponse getAverageDealTimeline() {
        List<Object[]> rows = historyRepository.findAverageDaysPerStageName();
        List<DealDtos.AverageDealTimelineItem> items = new ArrayList<>();
        for (Object[] row : rows) {
            String stageName = (String) row[0];
            Number avgNum = (Number) row[1];
            Double avgDays = avgNum != null ? avgNum.doubleValue() : null;
            Long visitCount = row[2] instanceof Number ? ((Number) row[2]).longValue() : 0L;
            items.add(new DealDtos.AverageDealTimelineItem(stageName, avgDays, visitCount));
        }
        return new DealDtos.AverageDealTimelineResponse(items);
    }

    /**
     * Get average deal timeline per month: for each month (yyyy-MM), average days in each stage (by stage name).
     * Month is the year-month when the deal exited the stage.
     */
    public DealDtos.AverageDealTimelinePerMonthResponse getAverageDealTimelinePerMonth() {
        List<Object[]> rows = historyRepository.findAverageDaysPerStageNamePerMonth();
        Map<String, List<DealDtos.AverageDealTimelineItem>> byMonth = new TreeMap<>();
        for (Object[] row : rows) {
            String month = (String) row[0];
            String stageName = (String) row[1];
            Number avgNum = (Number) row[2];
            Double avgDays = avgNum != null ? avgNum.doubleValue() : null;
            Long visitCount = row[3] instanceof Number ? ((Number) row[3]).longValue() : 0L;
            byMonth.computeIfAbsent(month, k -> new ArrayList<>())
                .add(new DealDtos.AverageDealTimelineItem(stageName, avgDays, visitCount));
        }
        List<DealDtos.AverageDealTimelinePerMonth> result = new ArrayList<>();
        for (Map.Entry<String, List<DealDtos.AverageDealTimelineItem>> e : byMonth.entrySet()) {
            result.add(new DealDtos.AverageDealTimelinePerMonth(e.getKey(), e.getValue()));
        }
        return new DealDtos.AverageDealTimelinePerMonthResponse(result);
    }
}

