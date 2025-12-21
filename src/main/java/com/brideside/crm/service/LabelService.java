package com.brideside.crm.service;

import com.brideside.crm.dto.LabelDtos;
import com.brideside.crm.entity.Label;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.LabelRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class LabelService {

    @Autowired
    private LabelRepository labelRepository;

    public List<LabelDtos.Response> findAll() {
        return labelRepository.findAll().stream()
                .map(this::toResponse)
                .collect(Collectors.toList());
    }

    public LabelDtos.Response findById(Long id) {
        Label label = labelRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Label not found with id: " + id));
        return toResponse(label);
    }

    public List<LabelDtos.Response> search(String query) {
        return labelRepository.findByNameContainingIgnoreCase(query).stream()
                .map(this::toResponse)
                .collect(Collectors.toList());
    }

    @Transactional
    public LabelDtos.Response create(LabelDtos.CreateRequest request) {
        if (request.name == null || request.name.trim().isEmpty()) {
            throw new BadRequestException("Label name is required");
        }
        if (labelRepository.existsByName(request.name.trim())) {
            throw new BadRequestException("Label with name '" + request.name + "' already exists");
        }

        Label label = new Label();
        label.setName(request.name.trim());
        label.setColor(request.color);

        Label saved = labelRepository.save(label);
        return toResponse(saved);
    }

    @Transactional
    public LabelDtos.Response update(Long id, LabelDtos.UpdateRequest request) {
        Label label = labelRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Label not found with id: " + id));

        if (request.name != null && !request.name.trim().isEmpty()) {
            // Check if another label already has this name
            labelRepository.findByName(request.name.trim())
                    .ifPresent(existing -> {
                        if (!existing.getId().equals(id)) {
                            throw new BadRequestException("Label with name '" + request.name + "' already exists");
                        }
                    });
            label.setName(request.name.trim());
        }
        if (request.color != null) {
            label.setColor(request.color);
        }

        Label saved = labelRepository.save(label);
        return toResponse(saved);
    }

    @Transactional
    public void delete(Long id) {
        if (!labelRepository.existsById(id)) {
            throw new ResourceNotFoundException("Label not found with id: " + id);
        }
        labelRepository.deleteById(id);
    }

    public Set<Label> findByIds(List<Long> ids) {
        if (ids == null || ids.isEmpty()) {
            return Set.of();
        }
        List<Label> labels = labelRepository.findAllById(ids);
        
        // Filter out soft-deleted labels
        List<Label> activeLabels = labels.stream()
                .filter(label -> label.getIsDeleted() == null || !label.getIsDeleted())
                .collect(Collectors.toList());
        
        // Check if all requested labels were found and are not deleted
        if (activeLabels.size() != ids.size()) {
            // Find which labels are missing
            List<Long> foundIds = activeLabels.stream()
                    .map(Label::getId)
                    .collect(Collectors.toList());
            List<Long> missingIds = ids.stream()
                    .filter(id -> !foundIds.contains(id))
                    .collect(Collectors.toList());
            
            if (missingIds.size() == 1) {
                throw new ResourceNotFoundException("Label not found with id: " + missingIds.get(0));
            } else {
                throw new ResourceNotFoundException("Labels not found with ids: " + missingIds);
            }
        }
        
        return Set.copyOf(activeLabels);
    }

    private LabelDtos.Response toResponse(Label label) {
        return new LabelDtos.Response(
                label.getId(),
                label.getName(),
                label.getColor(),
                label.getCreatedAt(),
                label.getUpdatedAt()
        );
    }
}

