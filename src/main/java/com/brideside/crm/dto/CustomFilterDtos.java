package com.brideside.crm.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

public class CustomFilterDtos {
    
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class FilterCondition {
        @JsonProperty("field")
        private String field;
        
        @JsonProperty("operator")
        private String operator;
        
        @JsonProperty("value")
        private String value;
    }
    
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SaveFilterRequest {
        @JsonProperty("name")
        private String name;
        
        @JsonProperty("conditions")
        private List<FilterCondition> conditions;
    }
    
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class FilterResponse {
        @JsonProperty("name")
        private String name;
        
        @JsonProperty("conditions")
        private List<FilterCondition> conditions;
    }
}
