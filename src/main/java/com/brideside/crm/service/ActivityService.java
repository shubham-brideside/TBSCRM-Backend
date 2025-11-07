package com.brideside.crm.service;

import com.brideside.crm.dto.ActivityDTO;
import com.brideside.crm.entity.Activity;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.mapper.ActivityMapper;
import com.brideside.crm.repository.ActivityRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class ActivityService {
    private final ActivityRepository repository;

    public ActivityService(ActivityRepository repository) {
        this.repository = repository;
    }

    public ActivityDTO create(ActivityDTO dto) {
        // Validate required fields
        if (dto.getSubject() == null || dto.getSubject().trim().isEmpty()) {
            throw new BadRequestException("Subject is required");
        }
        
        // personId and dealId are optional - will be null by default
        // This allows creating activities without linking to persons/deals initially
        // When deal page is integrated, these can be set via frontend
        // For now, frontend doesn't need to send these fields
        
        // Normalize personId and dealId: if 0 or negative, set to null
        // Frontend might send 0 or empty string, we treat it as null
        if (dto.getPersonId() != null && dto.getPersonId() <= 0) {
            dto.setPersonId(null);
        }
        if (dto.getDealId() != null && dto.getDealId() <= 0) {
            dto.setDealId(null);
        }
        
        Activity e = new Activity();
        ActivityMapper.updateEntity(dto, e);
        // personId and dealId will be null by default if not provided in DTO
        // Mapper handles setting them to null explicitly
        
        try {
            return ActivityMapper.toDto(repository.save(e));
        } catch (Exception ex) {
            // Catch FK constraint violations and provide clearer error
            // Only throw if personId/dealId were actually provided (not null)
            String msg = ex.getMessage();
            if (msg != null && msg.contains("foreign key constraint")) {
                if (msg.contains("deal_id") && dto.getDealId() != null) {
                    throw new BadRequestException("Deal ID " + dto.getDealId() + " does not exist. Please provide a valid deal ID.");
                } else if (msg.contains("person_id") && dto.getPersonId() != null) {
                    throw new BadRequestException("Person ID " + dto.getPersonId() + " does not exist. Please provide a valid person ID.");
                }
            }
            throw ex;
        }
    }

    public ActivityDTO update(Long id, ActivityDTO dto) {
        Activity e = repository.findById(id).orElseThrow(() -> new IllegalArgumentException("Activity not found: " + id));
        ActivityMapper.updateEntity(dto, e);
        return ActivityMapper.toDto(repository.save(e));
    }

    public void delete(Long id) { repository.deleteById(id); }

    public ActivityDTO markDone(Long id, boolean done) {
        Activity e = repository.findById(id).orElseThrow(() -> new IllegalArgumentException("Activity not found: " + id));
        e.setDone(done);
        return ActivityMapper.toDto(repository.save(e));
    }

    public Page<ActivityDTO> list(Long personId,
                                  String dateFrom,
                                  String dateTo,
                                  String assignedUser,
                                  String category,
                                  String status,
                                  String callType,
                                  Boolean done,
                                  Pageable pageable) {
        Specification<Activity> spec = Specification.where(null);

        if (personId != null) {
            spec = spec.and((root, q, cb) -> cb.equal(root.get("personId"), personId));
        }
        if (assignedUser != null && !assignedUser.isBlank()) {
            spec = spec.and((root, q, cb) -> cb.like(cb.lower(root.get("assignedUser")), "%" + assignedUser.toLowerCase() + "%"));
        }
        if (category != null && !category.isBlank()) {
            // Convert string to enum (case-insensitive)
            try {
                Activity.ActivityCategory categoryEnum = Activity.ActivityCategory.valueOf(category.toUpperCase().replace(" ", "_"));
                spec = spec.and((root, q, cb) -> cb.equal(root.get("category"), categoryEnum));
            } catch (IllegalArgumentException e) {
                // Invalid category value, skip filter
            }
        }
        if (status != null && !status.isBlank()) {
            // Convert string to enum (case-insensitive)
            try {
                Activity.ActivityStatus statusEnum = Activity.ActivityStatus.valueOf(status.toUpperCase().replace(" ", "_"));
                spec = spec.and((root, q, cb) -> cb.equal(root.get("status"), statusEnum));
            } catch (IllegalArgumentException e) {
                // Invalid status value, skip filter
            }
        }
        if (callType != null && !callType.isBlank()) {
            // Convert string to enum (case-insensitive)
            try {
                Activity.CallType callTypeEnum = Activity.CallType.valueOf(callType.toUpperCase().replace(" ", "_"));
                spec = spec.and((root, q, cb) -> cb.equal(root.get("callType"), callTypeEnum));
            } catch (IllegalArgumentException e) {
                // Invalid callType value, skip filter
            }
        }
        if (done != null) {
            spec = spec.and((root, q, cb) -> cb.equal(root.get("done"), done));
        }
        // date range is on dd/MM/yyyy string; simple lexical compare works if consistent format
        if (dateFrom != null && !dateFrom.isBlank()) {
            String from = dateFrom.trim();
            spec = spec.and((root, q, cb) -> cb.greaterThanOrEqualTo(root.get("date"), from));
        }
        if (dateTo != null && !dateTo.isBlank()) {
            String to = dateTo.trim();
            spec = spec.and((root, q, cb) -> cb.lessThanOrEqualTo(root.get("date"), to));
        }

        return repository.findAll(spec, pageable).map(ActivityMapper::toDto);
    }

    public Optional<ActivityDTO> get(Long id) {
        return repository.findById(id).map(ActivityMapper::toDto);
    }
}


