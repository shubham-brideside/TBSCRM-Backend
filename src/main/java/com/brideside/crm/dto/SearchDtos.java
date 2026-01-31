package com.brideside.crm.dto;

import java.util.List;

public class SearchDtos {
    
    public static class GlobalSearchResponse {
        private List<PersonDTO> persons;
        private List<DealResponse> deals;
        private int personsCount;
        private int dealsCount;
        private int totalCount;
        
        public GlobalSearchResponse() {
        }
        
        public GlobalSearchResponse(List<PersonDTO> persons, List<DealResponse> deals) {
            this.persons = persons;
            this.deals = deals;
            this.personsCount = persons != null ? persons.size() : 0;
            this.dealsCount = deals != null ? deals.size() : 0;
            this.totalCount = this.personsCount + this.dealsCount;
        }
        
        public List<PersonDTO> getPersons() {
            return persons;
        }
        
        public void setPersons(List<PersonDTO> persons) {
            this.persons = persons;
            this.personsCount = persons != null ? persons.size() : 0;
            this.totalCount = this.personsCount + this.dealsCount;
        }
        
        public List<DealResponse> getDeals() {
            return deals;
        }
        
        public void setDeals(List<DealResponse> deals) {
            this.deals = deals;
            this.dealsCount = deals != null ? deals.size() : 0;
            this.totalCount = this.personsCount + this.dealsCount;
        }
        
        public int getPersonsCount() {
            return personsCount;
        }
        
        public void setPersonsCount(int personsCount) {
            this.personsCount = personsCount;
            this.totalCount = this.personsCount + this.dealsCount;
        }
        
        public int getDealsCount() {
            return dealsCount;
        }
        
        public void setDealsCount(int dealsCount) {
            this.dealsCount = dealsCount;
            this.totalCount = this.personsCount + this.dealsCount;
        }
        
        public int getTotalCount() {
            return totalCount;
        }
        
        public void setTotalCount(int totalCount) {
            this.totalCount = totalCount;
        }
    }
}

