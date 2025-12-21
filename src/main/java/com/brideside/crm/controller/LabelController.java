package com.brideside.crm.controller;

import com.brideside.crm.dto.LabelDtos;
import com.brideside.crm.service.LabelService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/labels")
public class LabelController {

    @Autowired
    private LabelService labelService;

    @GetMapping
    public ResponseEntity<List<LabelDtos.Response>> getAll(
            @RequestParam(required = false) String search) {
        if (search != null && !search.trim().isEmpty()) {
            return ResponseEntity.ok(labelService.search(search));
        }
        return ResponseEntity.ok(labelService.findAll());
    }

    @GetMapping("/{id}")
    public ResponseEntity<LabelDtos.Response> getById(@PathVariable Long id) {
        return ResponseEntity.ok(labelService.findById(id));
    }

    @PostMapping
    public ResponseEntity<LabelDtos.Response> create(@RequestBody LabelDtos.CreateRequest request) {
        return ResponseEntity.status(HttpStatus.CREATED).body(labelService.create(request));
    }

    @PutMapping("/{id}")
    public ResponseEntity<LabelDtos.Response> update(
            @PathVariable Long id,
            @RequestBody LabelDtos.UpdateRequest request) {
        return ResponseEntity.ok(labelService.update(id, request));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable Long id) {
        labelService.delete(id);
        return ResponseEntity.noContent().build();
    }
}

