package com.brideside.crm.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;

import java.util.List;

@Schema(description = "Response from OLA Maps Autocomplete API")
@JsonIgnoreProperties(ignoreUnknown = true)
public class VenueAutocompleteResponse {

    @Schema(description = "List of place predictions")
    @JsonProperty("predictions")
    private List<PlacePrediction> predictions;

    @Schema(description = "Status of the API response")
    @JsonProperty("status")
    private String status;

    public List<PlacePrediction> getPredictions() {
        return predictions;
    }

    public void setPredictions(List<PlacePrediction> predictions) {
        this.predictions = predictions;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    @Schema(description = "Place prediction details")
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class PlacePrediction {

        @Schema(description = "Place description/name")
        @JsonProperty("description")
        private String description;

        @Schema(description = "Place ID for getting details")
        @JsonProperty("place_id")
        private String placeId;

        @Schema(description = "Structured format of the place")
        @JsonProperty("structured_formatting")
        private StructuredFormatting structuredFormatting;

        @Schema(description = "Distance in meters (if location provided)")
        @JsonProperty("distance_meters")
        private Integer distanceMeters;

        @Schema(description = "Types/categories of the place")
        @JsonProperty("types")
        private List<String> types;

        public String getDescription() {
            return description;
        }

        public void setDescription(String description) {
            this.description = description;
        }

        public String getPlaceId() {
            return placeId;
        }

        public void setPlaceId(String placeId) {
            this.placeId = placeId;
        }

        public StructuredFormatting getStructuredFormatting() {
            return structuredFormatting;
        }

        public void setStructuredFormatting(StructuredFormatting structuredFormatting) {
            this.structuredFormatting = structuredFormatting;
        }

        public Integer getDistanceMeters() {
            return distanceMeters;
        }

        public void setDistanceMeters(Integer distanceMeters) {
            this.distanceMeters = distanceMeters;
        }

        public List<String> getTypes() {
            return types;
        }

        public void setTypes(List<String> types) {
            this.types = types;
        }
    }

    @Schema(description = "Structured formatting for place display")
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class StructuredFormatting {

        @Schema(description = "Main text")
        @JsonProperty("main_text")
        private String mainText;

        @Schema(description = "Secondary text")
        @JsonProperty("secondary_text")
        private String secondaryText;

        public String getMainText() {
            return mainText;
        }

        public void setMainText(String mainText) {
            this.mainText = mainText;
        }

        public String getSecondaryText() {
            return secondaryText;
        }

        public void setSecondaryText(String secondaryText) {
            this.secondaryText = secondaryText;
        }
    }
}


