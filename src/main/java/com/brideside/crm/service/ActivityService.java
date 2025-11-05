package com.brideside.crm.service;

import com.brideside.crm.dto.ActivityDTO;
import com.brideside.crm.entity.Activity;
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
        Activity e = new Activity();
        ActivityMapper.updateEntity(dto, e);
        return ActivityMapper.toDto(repository.save(e));
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
            spec = spec.and((root, q, cb) -> cb.equal(cb.lower(root.get("category")), category.toLowerCase()));
        }
        if (status != null && !status.isBlank()) {
            spec = spec.and((root, q, cb) -> cb.equal(cb.lower(root.get("status")), status.toLowerCase()));
        }
        if (callType != null && !callType.isBlank()) {
            spec = spec.and((root, q, cb) -> cb.equal(cb.lower(root.get("callType")), callType.toLowerCase()));
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


