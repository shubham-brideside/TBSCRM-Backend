package com.brideside.crm.dto;

import java.time.Instant;

public final class CalendarDtos {

    private CalendarDtos() {
    }

    public static class VendorEventResponse {
        public Long id;
        public Long organizationId;
        public String organizationName;
        public String googleEventId;
        public String summary;
        public String description;
        public Instant startAt;
        public Instant endAt;
        public boolean allDay;
        public String status;
        public Instant lastSyncedAt;
    }
}




