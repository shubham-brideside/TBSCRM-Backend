package com.brideside.crm.service;

import com.brideside.crm.dto.SimpleDtos;
import com.brideside.crm.entity.Pipeline;
import com.brideside.crm.entity.Stage;

import java.util.List;

public interface PipelineService {
    Pipeline create(SimpleDtos.PipelineCreate req);
    List<Pipeline> list();
    Stage addStage(SimpleDtos.StageCreate req);
    List<Stage> listStages(Long pipelineId);
}



