package com.brideside.crm.controller;

import com.brideside.crm.integration.places.OlaMapsService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/venues")
@Tag(name = "Venues", description = "Venue autocomplete APIs using OLA Maps")
public class VenueController {

    @Autowired
    private OlaMapsService olaMapsService;

    @GetMapping(value = "/autocomplete", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(
            summary = "Search venues using OLA Maps Autocomplete",
            description = "Returns raw JSON response from OLA Maps Autocomplete API based on the input query."
    )
    public ResponseEntity<?> autocomplete(
            @Parameter(description = "Search query text", required = true, example = "delhi")
            @RequestParam String input) {

        if (input == null || input.trim().isEmpty()) {
            return ResponseEntity.badRequest()
                .body("{\"error\": \"Input parameter is required and cannot be empty\"}");
        }

        try {
            String response = olaMapsService.autocompleteVenues(input.trim());
            return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_JSON)
                .body(response);
        } catch (IllegalArgumentException e) {
            return ResponseEntity.badRequest()
                .body("{\"error\": \"" + e.getMessage() + "\"}");
        } catch (IllegalStateException e) {
            return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE)
                .body("{\"error\": \"" + e.getMessage() + "\"}");
        } catch (RuntimeException e) {
            // Check if it's an authentication/authorization error
            String errorMessage = e.getMessage();
            if (errorMessage != null && (errorMessage.contains("401") || errorMessage.contains("authentication"))) {
                return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                    .body("{\"error\": \"OLA Maps API authentication failed: " + e.getMessage() + "\"}");
            }
            if (errorMessage != null && (errorMessage.contains("403") || errorMessage.contains("forbidden"))) {
                return ResponseEntity.status(HttpStatus.FORBIDDEN)
                    .body("{\"error\": \"OLA Maps API access forbidden: " + e.getMessage() + "\"}");
            }
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body("{\"error\": \"Failed to fetch venue suggestions: " + e.getMessage() + "\"}");
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                .body("{\"error\": \"An unexpected error occurred: " + e.getMessage() + "\"}");
        }
    }
}

