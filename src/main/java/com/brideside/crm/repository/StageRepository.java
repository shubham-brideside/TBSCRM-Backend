package com.brideside.crm.repository;

import com.brideside.crm.entity.Stage;
import com.brideside.crm.entity.Pipeline;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface StageRepository extends JpaRepository<Stage, Long> {
    List<Stage> findByPipelineOrderByOrderIndexAsc(Pipeline pipeline);
}



