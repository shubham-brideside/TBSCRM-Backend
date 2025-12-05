package com.brideside.crm.repository;

import com.brideside.crm.entity.Person;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.List;

public interface PersonRepository extends JpaRepository<Person, Long>, JpaSpecificationExecutor<Person> {
    List<Person> findByOwner_Id(Long ownerId);
    
    // Methods that exclude soft-deleted persons
    List<Person> findByIsDeletedFalse();
}

