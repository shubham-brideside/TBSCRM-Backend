package com.brideside.crm.exception;

import com.brideside.crm.dto.PersonDTO;

public class DuplicatePersonException extends BadRequestException {
    private final PersonDTO duplicatePerson;

    public DuplicatePersonException(String message, PersonDTO duplicatePerson) {
        super(message);
        this.duplicatePerson = duplicatePerson;
    }

    public PersonDTO getDuplicatePerson() {
        return duplicatePerson;
    }
}
