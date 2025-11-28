package com.brideside.crm.repository;

import com.brideside.crm.entity.VendorCalendarEvent;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.Instant;
import java.util.List;
import java.util.Optional;

public interface VendorCalendarEventRepository extends JpaRepository<VendorCalendarEvent, Long> {

    Optional<VendorCalendarEvent> findByGoogleEventId(String googleEventId);

    List<VendorCalendarEvent> findByOrganization_Id(Long organizationId);

    List<VendorCalendarEvent> findByOrganization_IdAndStartAtBetween(Long organizationId, Instant startAt, Instant endAt);

    List<VendorCalendarEvent> findByStartAtBetween(Instant startAt, Instant endAt);
}

