package com.brideside.crm.service;

import com.brideside.crm.dto.SimpleDtos;
import com.brideside.crm.entity.Person;

import java.util.List;

public interface PersonService {
    Person create(SimpleDtos.PersonCreate req);
    List<Person> list();
}



