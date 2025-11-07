package com.brideside.crm.config;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.springdoc.core.models.GroupedOpenApi;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SwaggerConfig {

    @Bean
    public OpenAPI customOpenAPI() {
        return new OpenAPI()
                .info(new Info()
                        .title("Brideside CRM Backend API")
                        .version("1.0.0")
                        .description("REST API for Brideside CRM Application with Role-Based Access Control")
                        .contact(new Contact()
                                .name("Brideside CRM Team")
                                .email("support@brideside.com"))
                        .license(new License()
                                .name("Apache 2.0")
                                .url("http://www.apache.org/licenses/LICENSE-2.0.html")))
                .components(new Components()
                        .addSecuritySchemes("Bearer Authentication",
                                new SecurityScheme()
                                        .type(SecurityScheme.Type.HTTP)
                                        .scheme("bearer")
                                        .bearerFormat("JWT")
                                        .description("Enter JWT token")));
    }

    @Bean
    public GroupedOpenApi personsApi() {
        return GroupedOpenApi.builder()
                .group("persons")
                .packagesToScan("com.brideside.crm.controller")
                .pathsToMatch("/api/persons", "/api/persons/**")
                .build();
    }

    @Bean
    public GroupedOpenApi activitiesApi() {
        return GroupedOpenApi.builder()
                .group("activities")
                .packagesToScan("com.brideside.crm.controller")
                .pathsToMatch("/api/activities/**")
                .build();
    }

    @Bean
    public GroupedOpenApi allApi() {
        return GroupedOpenApi.builder()
                .group("all")
                .packagesToScan("com.brideside.crm.controller")
                .pathsToMatch("/api/**")
                .build();
    }
}

