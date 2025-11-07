package com.brideside.crm.service.impl;

import com.brideside.crm.dto.SimpleDtos;
import com.brideside.crm.entity.Pipeline;
import com.brideside.crm.entity.Stage;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.PipelineRepository;
import com.brideside.crm.repository.StageRepository;
import com.brideside.crm.service.PipelineService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class PipelineServiceImpl implements PipelineService {

    @Autowired private PipelineRepository pipelineRepository;
    @Autowired private StageRepository stageRepository;

    @Override
    public Pipeline create(SimpleDtos.PipelineCreate req) {
        Pipeline p = new Pipeline();
        p.setName(req.name);
        p.setCategory(req.category);
        p.setTeam(req.team);
        return pipelineRepository.save(p);
    }

    @Override
    public List<Pipeline> list() { return pipelineRepository.findAll(); }

    @Override
    public Stage addStage(SimpleDtos.StageCreate req) {
        Pipeline p = pipelineRepository.findById(req.pipelineId)
                .orElseThrow(() -> new ResourceNotFoundException("Pipeline not found"));
        Stage s = new Stage();
        s.setPipeline(p);
        s.setName(req.name);
        s.setOrderIndex(req.orderIndex == null ? 0 : req.orderIndex);
        return stageRepository.save(s);
    }

    @Override
    public List<Stage> listStages(Long pipelineId) {
        Pipeline p = pipelineRepository.findById(pipelineId)
                .orElseThrow(() -> new ResourceNotFoundException("Pipeline not found"));
        return stageRepository.findByPipelineOrderByOrderIndexAsc(p);
    }
}



