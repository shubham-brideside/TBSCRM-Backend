package com.brideside.crm.config;

import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

@Component
public class DataInitializer implements CommandLineRunner {

    @Override
    public void run(String... args) throws Exception {
        // Data initialization disabled - roles will be created on-demand
        // when creating the first admin user or when needed
    }
}

