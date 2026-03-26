package com.brideside.crm.dto;

import java.time.Instant;
import java.util.List;

public final class CalendarDtos {

    private CalendarDtos() {
    }

    public static class VendorEventResponse {
        public Long id;
        public Long dealId;
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

    public static class DealEventTypeByDateItem {
        public String eventDate; // yyyy-MM-dd
        public String eventType;
    }

    public static class UpsertDealEventTypesByDateRequest {
        public List<DealEventTypeByDateItem> items;
    }
}




