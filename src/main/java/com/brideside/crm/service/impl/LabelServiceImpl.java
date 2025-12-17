package com.brideside.crm.service.impl;

import com.brideside.crm.dto.LabelDtos.CreateLabelRequest;
import com.brideside.crm.dto.LabelDtos.UpdateLabelRequest;
import com.brideside.crm.entity.Label;
import com.brideside.crm.repository.LabelRepository;
import com.brideside.crm.service.LabelService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Service
@Transactional
public class LabelServiceImpl implements LabelService {

    @Autowired
    private LabelRepository labelRepository;

    @Override
    public Label create(CreateLabelRequest request) {
        Label label = new Label();
        label.setName(request.getName().trim().toUpperCase());
        label.setColor(request.getColor() != null ? request.getColor() : "#94a3b8");
        label.setOrganizationId(request.getOrganizationId());
        label.setPipelineId(request.getPipelineId());
        label.setCreatedByUserId(request.getCreatedByUserId());
        label.setIsGlobal(request.getIsGlobal() != null ? request.getIsGlobal() : false);
        return labelRepository.save(label);
    }

    @Override
    @Transactional(readOnly = true)
    public List<Label> findAll() {
        return labelRepository.findAll();
    }

    @Override
    @Transactional(readOnly = true)
    public Optional<Label> findById(Long id) {
        return labelRepository.findById(id);
    }

    @Override
    @Transactional(readOnly = true)
    public List<Label> findByOrganizationId(Long organizationId) {
        return labelRepository.findByOrganizationIdOrGlobal(organizationId);
    }

    @Override
    @Transactional(readOnly = true)
    public List<Label> findByPipelineId(Long pipelineId) {
        return labelRepository.findByPipelineIdOrGlobal(pipelineId);
    }

    @Override
    @Transactional(readOnly = true)
    public List<Label> findGlobalLabels() {
        return labelRepository.findByIsGlobalTrue();
    }

    @Override
    public Label update(Long id, UpdateLabelRequest request) {
        Label label = labelRepository.findById(id)
                .orElseThrow(() -> new RuntimeException("Label not found with id: " + id));

        if (request.getName() != null && !request.getName().trim().isEmpty()) {
            label.setName(request.getName().trim().toUpperCase());
        }
        if (request.getColor() != null) {
            label.setColor(request.getColor());
        }
        if (request.getOrganizationId() != null) {
            label.setOrganizationId(request.getOrganizationId());
        }
        if (request.getPipelineId() != null) {
            label.setPipelineId(request.getPipelineId());
        }
        if (request.getIsGlobal() != null) {
            label.setIsGlobal(request.getIsGlobal());
        }

        return labelRepository.save(label);
    }

    @Override
    public void delete(Long id) {
        if (!labelRepository.existsById(id)) {
            throw new RuntimeException("Label not found with id: " + id);
        }
        labelRepository.deleteById(id);
    }

    @Override
    @Transactional(readOnly = true)
    public boolean existsById(Long id) {
        return labelRepository.existsById(id);
    }
}
