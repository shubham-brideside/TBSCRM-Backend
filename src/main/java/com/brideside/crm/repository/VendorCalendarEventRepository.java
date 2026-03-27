package com.brideside.crm.repository;

import com.brideside.crm.entity.VendorCalendarEvent;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.Instant;
import java.util.List;
import java.util.Optional;

public interface VendorCalendarEventRepository extends JpaRepository<VendorCalendarEvent, Long> {

    Optional<VendorCalendarEvent> findByGoogleEventId(String googleEventId);

    List<VendorCalendarEvent> findByOrganization_Id(Long organizationId);

    List<VendorCalendarEvent> findByOrganization_IdAndStartAtBetween(Long organizationId, Instant startAt, Instant endAt);

    List<VendorCalendarEvent> findByStartAtBetween(Instant startAt, Instant endAt);
    List<VendorCalendarEvent> findByDealId(Long dealId);

    @Modifying
    @Query("UPDATE VendorCalendarEvent v SET v.team = :team WHERE v.dealId = :dealId")
    int updateTeamByDealId(@Param("dealId") Long dealId, @Param("team") String team);

    @Modifying
    @Query("DELETE FROM VendorCalendarEvent v WHERE v.dealId = :dealId")
    int deleteByDealId(@Param("dealId") Long dealId);
}

