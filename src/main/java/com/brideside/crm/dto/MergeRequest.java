package com.brideside.crm.dto;

import java.util.List;

public class MergeRequest {
    private List<Long> duplicateIds;

    public List<Long> getDuplicateIds() { return duplicateIds; }
    public void setDuplicateIds(List<Long> duplicateIds) { this.duplicateIds = duplicateIds; }
}

