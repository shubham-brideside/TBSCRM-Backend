package com.brideside.crm.service;

import com.brideside.crm.dto.DealResponse;
import com.brideside.crm.dto.PersonDTO;
import com.brideside.crm.dto.SearchDtos;

public interface SearchService {
    /**
     * Global search across persons and deals
     * Searches by name, instagram_id, and phone_number
     * 
     * @param query Search term
     * @param limit Maximum number of results per entity type (default: 10)
     * @return GlobalSearchResponse containing persons and deals
     */
    SearchDtos.GlobalSearchResponse globalSearch(String query, Integer limit);
}

