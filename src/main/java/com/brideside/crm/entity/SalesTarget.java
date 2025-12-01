package com.brideside.crm.entity;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.YearMonth;
import java.util.HashSet;
import java.util.Set;

@Entity
@Table(name = "sales_targets", uniqueConstraints = {
        @UniqueConstraint(name = "uk_target_user_category_period",
                columnNames = {"user_id", "category", "period_type", "period_start"})
})
@NoArgsConstructor
@AllArgsConstructor
public class SalesTarget {

    public enum PeriodType {
        MONTHLY,
        QUARTERLY,
        HALF_YEARLY,
        YEARLY
    }

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @Enumerated(EnumType.STRING)
    @Column(name = "category", nullable = false, length = 64)
    private TargetCategory category;

    @Enumerated(EnumType.STRING)
    @Column(name = "period_type", nullable = false, length = 20)
    private PeriodType periodType;

    @Column(name = "period_start", nullable = false)
    private LocalDate periodStart;

    // Backward compatibility: month_start column (deprecated, kept for database compatibility)
    @Column(name = "month_start", nullable = false, insertable = true, updatable = true)
    private LocalDate monthStart;

    @Column(name = "target_amount", nullable = false, precision = 14, scale = 2)
    private BigDecimal targetAmount = BigDecimal.ZERO;

    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "sales_target_organizations",
            joinColumns = @JoinColumn(name = "target_id"),
            inverseJoinColumns = @JoinColumn(name = "organization_id")
    )
    private Set<Organization> organizations = new HashSet<>();

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private LocalDateTime updatedAt;

    @PrePersist
    @PreUpdate
    public void normalizePeriodStart() {
        if (this.periodStart != null) {
            this.periodStart = this.periodStart.withDayOfMonth(1);
            // Sync month_start for backward compatibility
            this.monthStart = this.periodStart;
        }
        if (this.monthStart == null && this.periodStart != null) {
            this.monthStart = this.periodStart;
        }
        if (this.targetAmount == null) {
            this.targetAmount = BigDecimal.ZERO;
        }
        if (this.organizations == null) {
            this.organizations = new HashSet<>();
        }
    }

    public YearMonth getYearMonth() {
        return periodStart != null ? YearMonth.from(periodStart) : null;
    }

    // Helper method for backward compatibility
    @Deprecated
    public LocalDate getMonthStart() {
        return monthStart != null ? monthStart : periodStart;
    }

    @Deprecated
    public void setMonthStart(LocalDate monthStart) {
        this.monthStart = monthStart;
        this.periodStart = monthStart;
        this.periodType = PeriodType.MONTHLY;
    }

    // Getters and setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public TargetCategory getCategory() {
        return category;
    }

    public void setCategory(TargetCategory category) {
        this.category = category;
    }

    public PeriodType getPeriodType() {
        return periodType;
    }

    public void setPeriodType(PeriodType periodType) {
        this.periodType = periodType;
    }

    public LocalDate getPeriodStart() {
        return periodStart;
    }

    public void setPeriodStart(LocalDate periodStart) {
        this.periodStart = periodStart;
    }

    public Set<Organization> getOrganizations() {
        return organizations;
    }

    public void setOrganizations(Set<Organization> organizations) {
        this.organizations = organizations != null ? organizations : new HashSet<>();
    }

    public BigDecimal getTargetAmount() {
        return targetAmount;
    }

    public void setTargetAmount(BigDecimal targetAmount) {
        this.targetAmount = targetAmount;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }
}

