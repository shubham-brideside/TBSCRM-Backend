package com.brideside.crm.repository;

import com.brideside.crm.entity.DealCalendarEventType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface DealCalendarEventTypeRepository extends JpaRepository<DealCalendarEventType, Long> {
    List<DealCalendarEventType> findByDeal_IdOrderByEventDateAsc(Long dealId);

    @Modifying
    @Query("DELETE FROM DealCalendarEventType d WHERE d.deal.id = :dealId")
    void deleteAllByDealId(@Param("dealId") Long dealId);
}
