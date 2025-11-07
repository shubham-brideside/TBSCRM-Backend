package com.brideside.crm.config;

import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.EnumDeserializer;
import com.fasterxml.jackson.databind.module.SimpleModule;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;

import java.io.IOException;

@Configuration
public class JacksonConfig {

    @Bean
    @Primary
    public ObjectMapper objectMapper(Jackson2ObjectMapperBuilder builder) {
        ObjectMapper mapper = builder.build();
        
        // Don't fail on unknown properties (frontend might send extra fields)
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        
        // Add case-insensitive enum deserialization module
        SimpleModule module = new SimpleModule();
        module.setDeserializerModifier(new CaseInsensitiveEnumDeserializerModifier());
        mapper.registerModule(module);
        
        return mapper;
    }
    
    /**
     * Custom deserializer modifier that makes all enum deserialization case-insensitive
     */
    private static class CaseInsensitiveEnumDeserializerModifier extends com.fasterxml.jackson.databind.deser.BeanDeserializerModifier {
        @Override
        public JsonDeserializer<?> modifyEnumDeserializer(com.fasterxml.jackson.databind.DeserializationConfig config,
                                                          com.fasterxml.jackson.databind.JavaType type,
                                                          com.fasterxml.jackson.databind.BeanDescription beanDesc,
                                                          JsonDeserializer<?> deserializer) {
            if (deserializer instanceof EnumDeserializer) {
                return new CaseInsensitiveEnumDeserializer((EnumDeserializer) deserializer);
            }
            return deserializer;
        }
    }
    
    /**
     * Case-insensitive enum deserializer
     */
    private static class CaseInsensitiveEnumDeserializer extends JsonDeserializer<Enum<?>> {
        private final EnumDeserializer delegate;
        private final Class<Enum<?>> enumClass;

        @SuppressWarnings("unchecked")
        public CaseInsensitiveEnumDeserializer(EnumDeserializer delegate) {
            this.delegate = delegate;
            // Get enum class from the delegate's handled type
            this.enumClass = (Class<Enum<?>>) delegate.handledType();
        }

        @Override
        public Enum<?> deserialize(com.fasterxml.jackson.core.JsonParser p, DeserializationContext ctxt) throws IOException {
            String value = p.getText();
            if (value == null || value.isEmpty()) {
                return null;
            }
            
            // Get all enum constants
            Enum<?>[] enumConstants = enumClass.getEnumConstants();
            if (enumConstants == null) {
                // Fallback to delegate if we can't get constants
                Object result = delegate.deserialize(p, ctxt);
                return result instanceof Enum ? (Enum<?>) result : null;
            }
            
            // First try exact match (case-sensitive) - fastest path
            for (Enum<?> enumConstant : enumConstants) {
                if (enumConstant.name().equals(value)) {
                    return enumConstant;
                }
            }
            
            // If exact match fails, try case-insensitive matching
            for (Enum<?> enumConstant : enumConstants) {
                if (enumConstant.name().equalsIgnoreCase(value)) {
                    return enumConstant;
                }
            }
            
            // If still not found, try with underscores/spaces normalized
            // (e.g., "Meeting Scheduler" -> "MEETING_SCHEDULER")
            String normalized = value.toUpperCase().replace(" ", "_");
            for (Enum<?> enumConstant : enumConstants) {
                if (enumConstant.name().equalsIgnoreCase(normalized)) {
                    return enumConstant;
                }
            }
            
            // If still not found, throw a clear exception
            String[] enumNames = new String[enumConstants.length];
            for (int i = 0; i < enumConstants.length; i++) {
                enumNames[i] = enumConstants[i].name();
            }
            throw new com.fasterxml.jackson.databind.exc.InvalidFormatException(
                p, "Cannot deserialize value of type " + enumClass.getName() + 
                " from String \"" + value + "\": not one of the values accepted for Enum class: " + 
                java.util.Arrays.toString(enumNames), p.getCurrentValue(), enumClass);
        }
    }
}

