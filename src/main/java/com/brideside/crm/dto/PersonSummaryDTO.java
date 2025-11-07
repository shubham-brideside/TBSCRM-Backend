package com.brideside.crm.dto;

public class PersonSummaryDTO {
    private PersonDTO person;
    private long dealsCount;

    public PersonDTO getPerson() { return person; }
    public void setPerson(PersonDTO person) { this.person = person; }

    public long getDealsCount() { return dealsCount; }
    public void setDealsCount(long dealsCount) { this.dealsCount = dealsCount; }
}


