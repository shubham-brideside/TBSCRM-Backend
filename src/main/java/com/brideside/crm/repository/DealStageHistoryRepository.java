package com.brideside.crm.repository;

import com.brideside.crm.entity.DealStageHistory;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
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

    /**
     * Average days a deal spends in each stage (by stage name), using only completed stage visits
     * (exited_at set or days_in_stage set). Result: [stageName, avgDays, visitCount].
     */
    @Query(value = "SELECT s.name AS stage_name, " +
        "AVG(COALESCE(dsh.days_in_stage, DATEDIFF(dsh.exited_at, dsh.entered_at))) AS avg_days, " +
        "COUNT(*) AS visit_count " +
        "FROM deal_stage_history dsh " +
        "INNER JOIN stages s ON dsh.stage_id = s.id " +
        "WHERE (dsh.exited_at IS NOT NULL OR dsh.days_in_stage IS NOT NULL) " +
        "GROUP BY s.name", nativeQuery = true)
    List<Object[]> findAverageDaysPerStageName();

    /**
     * Average days per stage name per month (month = when the deal exited the stage).
     * Result: [month (yyyy-MM), stageName, avgDays, visitCount].
     */
    @Query(value = "SELECT DATE_FORMAT(dsh.exited_at, '%Y-%m') AS month, " +
        "s.name AS stage_name, " +
        "AVG(COALESCE(dsh.days_in_stage, DATEDIFF(dsh.exited_at, dsh.entered_at))) AS avg_days, " +
        "COUNT(*) AS visit_count " +
        "FROM deal_stage_history dsh " +
        "INNER JOIN stages s ON dsh.stage_id = s.id " +
        "WHERE dsh.exited_at IS NOT NULL " +
        "GROUP BY DATE_FORMAT(dsh.exited_at, '%Y-%m'), s.name " +
        "ORDER BY month, s.name", nativeQuery = true)
    List<Object[]> findAverageDaysPerStageNamePerMonth();
}

