package com.brideside.crm.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.HashMap;
import java.util.Map;

@RestController
public class HomeController {

    @GetMapping("/")
    public ResponseEntity<Map<String, Object>> home() {
        Map<String, Object> response = new HashMap<>();
        response.put("message", "Welcome to Brideside CRM Backend API");
        response.put("status", "running");
        response.put("documentation", "/swagger-ui.html");
        response.put("apiDocs", "/api-docs");
        response.put("frontend", "/frontend/index.html");
        return ResponseEntity.ok(response);
    }
}
