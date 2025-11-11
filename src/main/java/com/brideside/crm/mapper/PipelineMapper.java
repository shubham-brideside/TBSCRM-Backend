package com.brideside.crm.mapper;

import com.brideside.crm.dto.PipelineDtos;
import com.brideside.crm.entity.Pipeline;
import com.brideside.crm.entity.Stage;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public final class PipelineMapper {
    private PipelineMapper() {
    }

    public static PipelineDtos.PipelineResponse toPipelineResponse(Pipeline pipeline,
                                                                   List<Stage> stages,
                                                                   boolean includeStages) {
        PipelineDtos.PipelineResponse response = new PipelineDtos.PipelineResponse();
        response.setId(pipeline.getId());
        response.setName(pipeline.getName());
        response.setCategory(pipeline.getCategory());
        if (pipeline.getTeam() != null) {
            response.setTeamId(pipeline.getTeam().getId());
            PipelineDtos.TeamSummary team = new PipelineDtos.TeamSummary();
            team.setId(pipeline.getTeam().getId());
            team.setName(pipeline.getTeam().getName());
            response.setTeam(team);
        } else {
            response.setTeamId(null);
        }
        if (pipeline.getOrganization() != null) {
            PipelineDtos.OrganizationSummary org = new PipelineDtos.OrganizationSummary();
            org.setId(pipeline.getOrganization().getId());
            org.setName(pipeline.getOrganization().getName());
            response.setOrganization(org);
        }
        response.setDeleted(Boolean.TRUE.equals(pipeline.getDeleted()));
        response.setCreatedAt(pipeline.getCreatedAt());
        response.setUpdatedAt(pipeline.getUpdatedAt());
        if (includeStages) {
            List<PipelineDtos.StageResponse> stageResponses = stages.stream()
                    .sorted(Comparator.comparing(Stage::getOrderIndex))
                    .map(PipelineMapper::toStageResponse)
                    .collect(Collectors.toList());
            response.setStages(stageResponses);
        }
        return response;
    }

    public static PipelineDtos.StageResponse toStageResponse(Stage stage) {
        PipelineDtos.StageResponse response = new PipelineDtos.StageResponse();
        response.setId(stage.getId());
        response.setPipelineId(stage.getPipeline().getId());
        response.setName(stage.getName());
        response.setOrder(stage.getOrderIndex());
        response.setProbability(stage.getProbability());
        response.setActive(Boolean.TRUE.equals(stage.getActive()));
        response.setCreatedAt(stage.getCreatedAt());
        response.setUpdatedAt(stage.getUpdatedAt());
        return response;
    }
}


