package com.brideside.crm.dto;

import java.time.LocalDateTime;
import java.util.List;

public class LabelDtos {

    public static class CreateRequest {
        public String name;
        public String color; // Hex color code, e.g., "#FF5733"
    }

    public static class UpdateRequest {
        public String name;
        public String color;
    }

    public static class Response {
        public Long id;
        public String name;
        public String color;
        public LocalDateTime createdAt;
        public LocalDateTime updatedAt;

        public Response() {}

        public Response(Long id, String name, String color, LocalDateTime createdAt, LocalDateTime updatedAt) {
            this.id = id;
            this.name = name;
            this.color = color;
            this.createdAt = createdAt;
            this.updatedAt = updatedAt;
        }
    }

    public static class BulkAssignRequest {
        public List<Long> labelIds;
    }
}

