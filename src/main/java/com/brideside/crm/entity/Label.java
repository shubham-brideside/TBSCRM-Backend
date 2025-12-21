package com.brideside.crm.entity;

import jakarta.persistence.*;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDateTime;

@Entity
@Table(name = "labels")
public class Label {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, length = 100)
    private String name;

    @Column(length = 50, nullable = true)
    private String code; // Optional code field for backward compatibility

    @Column(length = 7)
    private String color; // Hex color code, e.g., "#FF5733"

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    @Column(name = "is_deleted", nullable = false)
    private Boolean isDeleted = Boolean.FALSE;

    @PrePersist
    public void prePersist() {
        // Set default code if null (generate from name or use empty string)
        if (this.code == null && this.name != null) {
            // Generate code from name: convert to uppercase and replace spaces with underscores
            this.code = this.name.toUpperCase().replaceAll("\\s+", "_");
        } else if (this.code == null) {
            // Fallback to empty string if name is also null
            this.code = "";
        }
        // Set default is_deleted if null
        if (this.isDeleted == null) {
            this.isDeleted = Boolean.FALSE;
        }
    }

    public Label() {}

    public Label(String name, String color) {
        this.name = name;
        this.color = color;
    }

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    public String getCode() { return code; }
    public void setCode(String code) { this.code = code; }

    public String getColor() { return color; }
    public void setColor(String color) { this.color = color; }

    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }

    public LocalDateTime getUpdatedAt() { return updatedAt; }
    public void setUpdatedAt(LocalDateTime updatedAt) { this.updatedAt = updatedAt; }

    public Boolean getIsDeleted() { return isDeleted; }
    public void setIsDeleted(Boolean isDeleted) { this.isDeleted = isDeleted; }
}

