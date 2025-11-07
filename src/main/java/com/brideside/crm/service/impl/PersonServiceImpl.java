package com.brideside.crm.service.impl;

import com.brideside.crm.dto.SimpleDtos;
import com.brideside.crm.entity.Person;
import com.brideside.crm.repository.PersonRepository;
import com.brideside.crm.service.PersonService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class PersonServiceImpl implements PersonService {

    @Autowired private PersonRepository personRepository;

    @Override
    public Person create(SimpleDtos.PersonCreate req) {
        Person p = new Person();
        p.setName(req.name);
        p.setPhoneNum(req.phoneNum);
        p.setWeddingCity(req.weddingCity);
        return personRepository.save(p);
    }

    @Override
    public List<Person> list() { return personRepository.findAll(); }
}



