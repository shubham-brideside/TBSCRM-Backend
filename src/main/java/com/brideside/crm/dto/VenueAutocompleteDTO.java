package com.brideside.crm.dto;

import io.swagger.v3.oas.annotations.media.Schema;

@Schema(description = "Simplified venue autocomplete result for frontend")
public class VenueAutocompleteDTO {

    @Schema(description = "Place description/name to display")
    private String description;

    @Schema(description = "Place ID (can be used for place details)")
    private String placeId;

    @Schema(description = "Main text for display")
    private String mainText;

    @Schema(description = "Secondary text (address)")
    private String secondaryText;

    @Schema(description = "Distance in meters (if location provided)")
    private Integer distanceMeters;

    public VenueAutocompleteDTO() {
    }

    public VenueAutocompleteDTO(String description, String placeId, String mainText, String secondaryText, Integer distanceMeters) {
        this.description = description;
        this.placeId = placeId;
        this.mainText = mainText;
        this.secondaryText = secondaryText;
        this.distanceMeters = distanceMeters;
    }

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

    public Integer getDistanceMeters() {
        return distanceMeters;
    }

    public void setDistanceMeters(Integer distanceMeters) {
        this.distanceMeters = distanceMeters;
    }
}


