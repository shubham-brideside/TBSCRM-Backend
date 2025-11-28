package com.brideside.crm.security;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import java.util.Arrays;
import java.util.List;

@Configuration
@EnableWebSecurity
@EnableMethodSecurity
public class SecurityConfig {

    @Autowired
    private CustomUserDetailsService userDetailsService;

    @Autowired
    private JwtAuthenticationFilter jwtAuthenticationFilter;

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

    @Bean
    public DaoAuthenticationProvider authenticationProvider() {
        DaoAuthenticationProvider authProvider = new DaoAuthenticationProvider();
        authProvider.setUserDetailsService(userDetailsService);
        authProvider.setPasswordEncoder(passwordEncoder());
        return authProvider;
    }

    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration authConfig) throws Exception {
        return authConfig.getAuthenticationManager();
    }

    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http
                .csrf(csrf -> csrf.disable())
                .cors(cors -> cors.configurationSource(corsConfigurationSource()))
                .sessionManagement(session -> session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                .headers(headers -> headers
                        .frameOptions(frame -> frame.deny())
                        .httpStrictTransportSecurity(hsts -> hsts
                                .maxAgeInSeconds(31536000)))
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers(org.springframework.http.HttpMethod.OPTIONS, "/**").permitAll() // Allow all OPTIONS requests for CORS preflight
                        .requestMatchers("/").permitAll() // Allow root path
                        .requestMatchers("/index.html").permitAll() // Allow index page
                        .requestMatchers("/actuator/**").permitAll() // Allow health check endpoints for Azure
                        .requestMatchers("/api/auth/**").permitAll()
                        .requestMatchers("/api/users/set-password").permitAll() // Allow password setting via invitation token
                        .requestMatchers("/api/users/accept-invitation").permitAll() // Allow invitation verification
                        .requestMatchers("/api/admin/create-admin").permitAll()
                        .requestMatchers("/api/admin/activate-admin").permitAll()
                        .requestMatchers("/api/admin/test-email").permitAll() // Added for email testing
                        .requestMatchers("/api/persons/**").permitAll() // Temporarily open for testing
                        .requestMatchers("/api/activities/**").permitAll() // Temporarily open for testing
                        .requestMatchers("/swagger-ui/**", "/swagger-ui.html", "/api-docs/**", "/v3/api-docs/**").permitAll()
                        .requestMatchers("/frontend/**").permitAll()
                        .anyRequest().authenticated()
                );

        http.authenticationProvider(authenticationProvider());
        http.addFilterBefore(jwtAuthenticationFilter, UsernamePasswordAuthenticationFilter.class);

        return http.build();
    }

    @Value("${app.frontend.base-url:https://tbscrm-frontend-cjcyene4bvc3d2gs.canadacentral-01.azurewebsites.net}")
    private String frontendBaseUrl;
    
    @Bean
    public CorsConfigurationSource corsConfigurationSource() {
        CorsConfiguration configuration = new CorsConfiguration();
        
        // Allow specific frontend origin (can use allowCredentials with specific origins)
        // Make sure to include both http and https versions, and handle trailing slashes
        String frontendUrl = frontendBaseUrl.endsWith("/") 
            ? frontendBaseUrl.substring(0, frontendBaseUrl.length() - 1) 
            : frontendBaseUrl;
        
        List<String> allowedOrigins = Arrays.asList(
            frontendUrl,
            frontendUrl.replace("https://", "http://"),  // Also allow http version if needed
            "http://localhost:3000",  // For local development
            "http://localhost:5173",  // For Vite dev server
            "http://localhost:8080"   // For local backend testing
        );
        
        // Log the configured origins for debugging
        System.out.println("CORS Configuration - Frontend URL: " + frontendUrl);
        System.out.println("CORS Configuration - Allowed Origins: " + allowedOrigins);
        
        configuration.setAllowedOrigins(allowedOrigins);
        
        // Allow credentials when using specific origins
        configuration.setAllowCredentials(true);
        
        // Allow all methods
        configuration.setAllowedMethods(Arrays.asList("GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS", "HEAD"));
        
        // Allow all headers (including Authorization)
        configuration.setAllowedHeaders(Arrays.asList("*"));
        
        // Expose headers that frontend needs
        configuration.setExposedHeaders(Arrays.asList("Authorization", "Content-Type", "X-Requested-With"));
        
        // Cache preflight response for 1 hour
        configuration.setMaxAge(3600L);
        
        UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        source.registerCorsConfiguration("/**", configuration);
        return source;
    }
}

