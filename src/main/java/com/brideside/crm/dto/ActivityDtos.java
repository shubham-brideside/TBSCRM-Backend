package com.brideside.crm.dto;

import java.time.LocalDateTime;

public class ActivityDtos {
    public static class CreateRequest {
        public Long dealId;
        public String type;
        public LocalDateTime dateTime;
        public String status;
        public Long durationMinutes;
    }
}



