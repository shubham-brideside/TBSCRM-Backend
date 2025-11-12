package com.brideside.crm.dto;

import com.brideside.crm.entity.Activity;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class ActivityDtos {

    private ActivityDtos() {}

    public static class CreateRequest {
        public Long dealId;
        public String type;
        public LocalDateTime dateTime;
        public String status;
        public Long durationMinutes;
    }

    public static class CategoryOption {
        private String code;
        private String label;

        public CategoryOption() {}

        public CategoryOption(String code, String label) {
            this.code = code;
            this.label = label;
        }

        public String getCode() {
            return code;
        }

        public void setCode(String code) {
            this.code = code;
        }

        public String getLabel() {
            return label;
        }

        public void setLabel(String label) {
            this.label = label;
        }
    }

    public static List<CategoryOption> allCategoryOptions() {
        return Arrays.stream(Activity.ActivityCategory.values())
                .map(cat -> new CategoryOption(cat.name(), prettify(cat.name())))
                .collect(Collectors.toList());
    }

    private static String prettify(String code) {
        String lower = code.toLowerCase().replace('_', ' ');
        return Arrays.stream(lower.split(" "))
                .map(word -> word.isEmpty() ? word : Character.toUpperCase(word.charAt(0)) + word.substring(1))
                .collect(Collectors.joining(" "));
    }
}

