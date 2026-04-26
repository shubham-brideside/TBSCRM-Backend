package com.brideside.crm.entity;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "roles")
@NoArgsConstructor
@AllArgsConstructor
public class Role {
    
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Enumerated(EnumType.STRING)
    @Column(nullable = false, unique = true)
    private RoleName name;
    
    @Column(length = 500)
    private String description;
    
    // Getters and Setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public RoleName getName() {
        return name;
    }

    public void setName(RoleName name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
    
    public enum RoleName {
        ADMIN,
        CATEGORY_MANAGER,
        SALES,
        PRESALES,
        /** TBS flow: pre-sales pipeline and org TBS Test (117). DB-safe ENUM-style name. */
        TBS_PRESALES,
        /** TBS flow: relationship manager pipeline and org TBS Test (117). */
        TBS_REL_MANAGER,
        /** TBS flow: service manager pipeline; org chosen from Revaah / TBS Planning / TBS Venue. */
        TBS_SVC_MANAGER
    }
}

