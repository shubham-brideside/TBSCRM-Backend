package com.brideside.crm.dto;

import jakarta.validation.constraints.Size;


import java.math.BigDecimal;
import java.util.List;

public final class EventPricingDtos {
    private EventPricingDtos() {
    }

    /** Pricing for one artist level (primary, senior, or junior) within an event row. */
    public static class ArtistPricing {
        private BigDecimal basePrice;
        private BigDecimal destinationPrice;
        private BigDecimal additionalMakeupPrice;
        private String availabilityAtStudio;
        private String policyNotes;
        private String currency;

        public BigDecimal getBasePrice() { return basePrice; }
        public void setBasePrice(BigDecimal basePrice) { this.basePrice = basePrice; }
        public BigDecimal getDestinationPrice() { return destinationPrice; }
        public void setDestinationPrice(BigDecimal destinationPrice) { this.destinationPrice = destinationPrice; }
        public BigDecimal getAdditionalMakeupPrice() { return additionalMakeupPrice; }
        public void setAdditionalMakeupPrice(BigDecimal additionalMakeupPrice) { this.additionalMakeupPrice = additionalMakeupPrice; }
        public String getAvailabilityAtStudio() { return availabilityAtStudio; }
        public void setAvailabilityAtStudio(String availabilityAtStudio) { this.availabilityAtStudio = availabilityAtStudio; }
        public String getPolicyNotes() { return policyNotes; }
        public void setPolicyNotes(String policyNotes) { this.policyNotes = policyNotes; }
        public String getCurrency() { return currency; }
        public void setCurrency(String currency) { this.currency = currency; }
    }

    /** One event/resource row with pricing per artist level. */
    public static class EventPricingRow {
        @Size(max = 100)
        private String code;
        @Size(max = 255)
        private String label;
        private ArtistPricing primary;
        private ArtistPricing senior;
        private ArtistPricing junior;

        public String getCode() { return code; }
        public void setCode(String code) { this.code = code; }
        public String getLabel() { return label; }
        public void setLabel(String label) { this.label = label; }
        public ArtistPricing getPrimary() { return primary; }
        public void setPrimary(ArtistPricing primary) { this.primary = primary; }
        public ArtistPricing getSenior() { return senior; }
        public void setSenior(ArtistPricing senior) { this.senior = senior; }
        public ArtistPricing getJunior() { return junior; }
        public void setJunior(ArtistPricing junior) { this.junior = junior; }
    }

    /** Request to save event pricing. */
    public static class EventPricingUpdateRequest {
        /** For Photography etc.: single session, primary only. Session = DEFAULT. */
        private List<EventPricingRow> eventPricing;
        /** For Makeup: Current session (Jan–Sep). */
        private List<EventPricingRow> eventPricingCurrent;
        /** For Makeup: Upcoming session (Oct–next Sep). */
        private List<EventPricingRow> eventPricingUpcoming;

        public List<EventPricingRow> getEventPricing() { return eventPricing; }
        public void setEventPricing(List<EventPricingRow> eventPricing) { this.eventPricing = eventPricing; }
        public List<EventPricingRow> getEventPricingCurrent() { return eventPricingCurrent; }
        public void setEventPricingCurrent(List<EventPricingRow> eventPricingCurrent) { this.eventPricingCurrent = eventPricingCurrent; }
        public List<EventPricingRow> getEventPricingUpcoming() { return eventPricingUpcoming; }
        public void setEventPricingUpcoming(List<EventPricingRow> eventPricingUpcoming) { this.eventPricingUpcoming = eventPricingUpcoming; }
    }
}
