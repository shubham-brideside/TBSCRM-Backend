package com.brideside.crm.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;

import java.util.Arrays;
import java.util.List;

@Configuration
public class CorsConfig {
    
    @Value("${app.frontend.base-url:https://tbscrm-frontend-cjcyene4bvc3d2gs.canadacentral-01.azurewebsites.net}")
    private String frontendBaseUrl;
    
    @Bean
    public CorsFilter corsFilter() {
        UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        CorsConfiguration config = new CorsConfiguration();
        
        // Allow specific frontend origin (can use allowCredentials with specific origins)
        List<String> allowedOrigins = Arrays.asList(
            frontendBaseUrl,
            "http://localhost:3000",  // For local development
            "http://localhost:5173",  // For Vite dev server
            "http://localhost:8080"   // For local backend testing
        );
        config.setAllowedOrigins(allowedOrigins);
        
        // Allow credentials when using specific origins
        config.setAllowCredentials(true);
        
        // Allow all methods
        config.setAllowedMethods(Arrays.asList("GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS", "HEAD"));
        
        // Allow all headers
        config.setAllowedHeaders(Arrays.asList("*"));
        
        // Expose headers that frontend needs
        config.setExposedHeaders(Arrays.asList("Authorization", "Content-Type"));
        
        // Cache preflight response for 1 hour
        config.setMaxAge(3600L);
        
        source.registerCorsConfiguration("/**", config);
        return new CorsFilter(source);
    }
}


