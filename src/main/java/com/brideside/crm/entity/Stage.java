package com.brideside.crm.entity;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "stages")
@NoArgsConstructor
@AllArgsConstructor
public class Stage {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "pipeline_id")
    private Pipeline pipeline;

    @Column(nullable = false)
    private String name;

    @Column(name = "stage_order", nullable = false)
    private Integer orderIndex;

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public Pipeline getPipeline() { return pipeline; }
    public void setPipeline(Pipeline pipeline) { this.pipeline = pipeline; }

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    public Integer getOrderIndex() { return orderIndex; }
    public void setOrderIndex(Integer orderIndex) { this.orderIndex = orderIndex; }
}



