package com.brideside.crm.dto;

import java.util.List;

public class PersonDtos {
    
    /**
     * Response DTO for persons with details endpoint
     * Contains paginated persons with their related deals and activities
     */
    public static class PersonsWithDetailsResponse {
        public List<PersonDTO> persons;
        public List<DealResponse> deals;
        public List<ActivityDTO> activities;
        public PaginationInfo pagination;
        public long totalPersons; // Total number of persons matching the filters (regardless of pagination)

        public PersonsWithDetailsResponse(List<PersonDTO> persons, 
                                         List<DealResponse> deals, 
                                         List<ActivityDTO> activities,
                                         PaginationInfo pagination,
                                         long totalPersons) {
            this.persons = persons;
            this.deals = deals;
            this.activities = activities;
            this.pagination = pagination;
            this.totalPersons = totalPersons;
        }
    }

    /**
     * Response DTO for single person with details endpoint
     * Contains person details with their related deals and activities
     */
    public static class PersonWithDetailsResponse {
        public PersonDTO person;
        public List<DealResponse> deals;
        public List<ActivityDTO> activities;

        public PersonWithDetailsResponse(PersonDTO person, 
                                        List<DealResponse> deals, 
                                        List<ActivityDTO> activities) {
            this.person = person;
            this.deals = deals;
            this.activities = activities;
        }
    }

    /**
     * Pagination metadata
     */
    public static class PaginationInfo {
        public int page;
        public int size;
        public long totalElements;
        public int totalPages;
        public boolean hasNext;
        public boolean hasPrevious;

        public PaginationInfo(int page, int size, long totalElements, int totalPages, 
                             boolean hasNext, boolean hasPrevious) {
            this.page = page;
            this.size = size;
            this.totalElements = totalElements;
            this.totalPages = totalPages;
            this.hasNext = hasNext;
            this.hasPrevious = hasPrevious;
        }
    }
}

