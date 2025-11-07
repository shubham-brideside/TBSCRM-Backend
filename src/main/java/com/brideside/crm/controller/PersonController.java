package com.brideside.crm.controller;

import com.brideside.crm.dto.ApiResponse;
import com.brideside.crm.dto.SimpleDtos;
import com.brideside.crm.entity.Person;
import com.brideside.crm.service.PersonService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/persons")
@Tag(name = "Persons", description = "Person/lead APIs")
public class PersonController {

    @Autowired private PersonService personService;

    @PostMapping
    @Operation(summary = "Create person")
    public ResponseEntity<ApiResponse<Person>> create(@Valid @RequestBody SimpleDtos.PersonCreate req) {
        return ResponseEntity.ok(ApiResponse.success("Person created", personService.create(req)));
    }

    @GetMapping
    @Operation(summary = "List persons")
    public ResponseEntity<ApiResponse<List<Person>>> list() {
        return ResponseEntity.ok(ApiResponse.success("Persons fetched", personService.list()));
    }
}



