package com.brideside.crm.service;

import com.brideside.crm.dto.ActivityDTO;
import com.brideside.crm.dto.ActivityDtos;
import com.brideside.crm.entity.Activity;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.mapper.ActivityMapper;
import com.brideside.crm.repository.ActivityRepository;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.Predicate;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
@Transactional(readOnly = true)
public class ActivityService {
    private final ActivityRepository repository;
    private final ActivityScopeService scopeService;

    public ActivityService(ActivityRepository repository,
                           ActivityScopeService scopeService) {
        this.repository = repository;
        this.scopeService = scopeService;
    }

    @Transactional
    public ActivityDTO create(ActivityDTO dto) {
        // Validate and trim required fields
        if (dto.getSubject() == null || dto.getSubject().trim().isEmpty()) {
            throw new BadRequestException("Subject is required");
        }
        // Trim subject before saving
        dto.setSubject(dto.getSubject().trim());
        
        // Set default category to ACTIVITY if not provided
        if (dto.getCategory() == null) {
            dto.setCategory(Activity.ActivityCategory.ACTIVITY);
        }
        
        // personId and dealId are optional; included only when > 0 (handled by frontend)
        // dateTime optional for now
        
        // Exclude done from create - managed via POST /api/activities/{id}/done
        // Legacy scheduleBy/callType fields are intentionally not exposed in the DTO anymore
        
        Activity e = new Activity();
        ActivityMapper.updateEntityForCreate(dto, e);
        // Ensure done defaults to false (entity default, but explicit for clarity)
        e.setDone(false);
        try {
            return ActivityMapper.toDto(repository.save(e));
        } catch (Exception ex) {
            // Catch FK constraint violations and provide clearer error
            String msg = ex.getMessage();
            if (msg != null && msg.contains("foreign key constraint")) {
                if (msg.contains("deal_id")) {
                    throw new BadRequestException("Deal ID " + dto.getDealId() + " does not exist. Please provide a valid deal ID.");
                } else if (msg.contains("person_id")) {
                    throw new BadRequestException("Person ID " + dto.getPersonId() + " does not exist. Please provide a valid person ID.");
                }
            }
            throw ex;
        }
    }

    @Transactional
    public ActivityDTO update(Long id, ActivityDTO dto) {
        Activity e = repository.findById(id).orElseThrow(() -> new IllegalArgumentException("Activity not found: " + id));
        // Trim subject if provided
        if (dto.getSubject() != null) {
            dto.setSubject(dto.getSubject().trim());
            if (dto.getSubject().isEmpty()) {
                throw new BadRequestException("Subject cannot be empty");
            }
        }
        ActivityMapper.updateEntity(dto, e);
        return ActivityMapper.toDto(repository.save(e));
    }

    @Transactional
    public void delete(Long id) { repository.deleteById(id); }

    @Transactional
    public ActivityDTO markDone(Long id, boolean done) {
        Activity e = repository.findById(id).orElseThrow(() -> new IllegalArgumentException("Activity not found: " + id));
        e.setDone(done);
        return ActivityMapper.toDto(repository.save(e));
    }

    public Page<ActivityDTO> list(Long personId,
                                  String dateFrom,
                                  String dateTo,
                                  String assignedUser,
                                  Long organizationId,
                                  Long assignedUserId,
                                  String category,
                                  String status,
                                  Boolean done,
                                  Pageable pageable) {
        Specification<Activity> spec = buildSpecification(
                personId, dateFrom, dateTo, assignedUser,
                organizationId, assignedUserId, category, status, done
        );

        // Fetch activities with organization and owner data loaded
        Page<Activity> activities = repository.findAll(spec, pageable);

        // Force load organizationRef and owner for each activity to avoid lazy loading issues
        activities.getContent().forEach(activity -> {
            if (activity.getOrganizationRef() != null) {
                activity.getOrganizationRef().getName();
                if (activity.getOrganizationRef().getOwner() != null) {
                    activity.getOrganizationRef().getOwner().getEmail();
                }
            }
        });

        return activities.map(ActivityMapper::toDto);
    }

    /**
     * Compute summary counts for dashboard cards using the same scope and filters as list().
     */
    public ActivityDtos.Summary summary(Long personId,
                                        String dateFrom,
                                        String dateTo,
                                        String assignedUser,
                                        Long organizationId,
                                        Long assignedUserId,
                                        String category,
                                        String status,
                                        Boolean done) {
        Specification<Activity> baseSpec = buildSpecification(
                personId, dateFrom, dateTo, assignedUser,
                organizationId, assignedUserId, category, status, done
        );

        ActivityDtos.Summary s = new ActivityDtos.Summary();
        // Total: all matching activities
        s.setTotal(repository.count(baseSpec));

        // Pending: status = PENDING
        Specification<Activity> pendingSpec = baseSpec.and((root, q, cb) ->
                cb.equal(root.get("status"), Activity.ActivityStatus.PENDING));
        s.setPending(repository.count(pendingSpec));

        // Completed: status = COMPLETED
        Specification<Activity> completedSpec = baseSpec.and((root, q, cb) ->
                cb.equal(root.get("status"), Activity.ActivityStatus.COMPLETED));
        s.setCompleted(repository.count(completedSpec));

        // Assign Call: category = CALL
        Specification<Activity> callSpec = baseSpec.and((root, q, cb) ->
                cb.equal(root.get("category"), Activity.ActivityCategory.CALL));
        s.setAssignCall(repository.count(callSpec));

        // Meeting scheduled: category = MEETING_SCHEDULER
        Specification<Activity> meetingSpec = baseSpec.and((root, q, cb) ->
                cb.equal(root.get("category"), Activity.ActivityCategory.MEETING_SCHEDULER));
        s.setMeetingScheduled(repository.count(meetingSpec));

        return s;
    }

    private Predicate orPredicates(CriteriaBuilder cb, List<Predicate> predicates) {
        if (predicates == null || predicates.isEmpty()) {
            return cb.disjunction();
        }
        if (predicates.size() == 1) {
            return predicates.get(0);
        }
        return cb.or(predicates.toArray(new Predicate[0]));
    }

    /**
     * Build the JPA Specification used for both list() and summary().
     * This ensures the main table and the summary cards are always consistent.
     */
    private Specification<Activity> buildSpecification(Long personId,
                                                       String dateFrom,
                                                       String dateTo,
                                                       String assignedUser,
                                                       Long organizationId,
                                                       Long assignedUserId,
                                                       String category,
                                                       String status,
                                                       Boolean done) {
        Specification<Activity> spec = Specification.where(null);

        // Apply role-based scoping first (by IDs)
        // An activity should show if it matches EITHER organization OR assigned user scope
        // For Sales users: Shows activities for their organizations (regardless of assignment)
        //                  OR activities assigned to them/their Presales team
        // For Presales users: Shows activities for organizations owned by their Sales manager
        //                     (regardless of assignment) OR activities assigned to themselves
        ActivityScopeService.Scope scope = scopeService.resolveScope();

        // Build OR condition: activity matches if organization or assigned user falls inside scope.
        boolean hasOrgIds = !scope.organizationIds().isEmpty();
        boolean hasOrgNames = !scope.organizationNames().isEmpty();
        boolean hasUserIds = !scope.assignedUserIds().isEmpty();
        boolean hasUserEmails = !scope.assignedUserEmails().isEmpty();

        boolean restrictsByOrg = hasOrgIds || hasOrgNames;
        boolean restrictsByUser = hasUserIds || hasUserEmails;

        if (restrictsByOrg || restrictsByUser) {
            Specification<Activity> scopeSpec = (root, q, cb) -> {
                List<Predicate> disjuncts = new ArrayList<>();

                if (restrictsByOrg) {
                    List<Predicate> orgPredicates = new ArrayList<>();
                    if (hasOrgIds) {
                        orgPredicates.add(root.get("organizationId").in(scope.organizationIds()));
                    }
                    if (hasOrgNames) {
                        orgPredicates.add(
                                cb.lower(root.get("organization")).in(scope.organizationNames()));
                    }
                    if (!orgPredicates.isEmpty()) {
                        disjuncts.add(orPredicates(cb, orgPredicates));
                    }
                }

                if (restrictsByUser) {
                    List<Predicate> userPredicates = new ArrayList<>();
                    if (hasUserIds) {
                        userPredicates.add(root.get("assignedUserId").in(scope.assignedUserIds()));
                    }
                    if (hasUserEmails) {
                        userPredicates.add(
                                cb.lower(root.get("assignedUser")).in(scope.assignedUserEmails()));
                    }
                    if (!userPredicates.isEmpty()) {
                        disjuncts.add(orPredicates(cb, userPredicates));
                    }
                }

                if (disjuncts.isEmpty()) {
                    return cb.conjunction();
                }
                return orPredicates(cb, disjuncts);
            };
            spec = spec.and(scopeSpec);
        }

        // Narrow by explicit IDs, but always inside scope
        if (organizationId != null) {
            spec = spec.and((root, q, cb) -> cb.equal(root.get("organizationId"), organizationId));
        }
        if (assignedUserId != null) {
            spec = spec.and((root, q, cb) -> cb.equal(root.get("assignedUserId"), assignedUserId));
        }

        if (personId != null) {
            spec = spec.and((root, q, cb) -> cb.equal(root.get("personId"), personId));
        }
        if (assignedUser != null && !assignedUser.isBlank()) {
            spec = spec.and((root, q, cb) -> cb.like(cb.lower(root.get("assignedUser")), "%" + assignedUser.toLowerCase() + "%"));
        }
        if (category != null && !category.isBlank()) {
            try {
                Activity.ActivityCategory categoryEnum = Activity.ActivityCategory.valueOf(category.toUpperCase().replace(" ", "_"));
                spec = spec.and((root, q, cb) -> cb.equal(root.get("category"), categoryEnum));
            } catch (IllegalArgumentException e) {
                // Invalid category value, skip filter
            }
        }
        if (status != null && !status.isBlank()) {
            try {
                Activity.ActivityStatus statusEnum = Activity.ActivityStatus.valueOf(status.toUpperCase().replace(" ", "_"));
                spec = spec.and((root, q, cb) -> cb.equal(root.get("status"), statusEnum));
            } catch (IllegalArgumentException e) {
                // Invalid status value, skip filter
            }
        }
        if (done != null) {
            spec = spec.and((root, q, cb) -> cb.equal(root.get("done"), done));
        }
        if (dateFrom != null && !dateFrom.isBlank()) {
            String from = dateFrom.trim();
            spec = spec.and((root, q, cb) -> cb.greaterThanOrEqualTo(root.get("date"), from));
        }
        if (dateTo != null && !dateTo.isBlank()) {
            String to = dateTo.trim();
            spec = spec.and((root, q, cb) -> cb.lessThanOrEqualTo(root.get("date"), to));
        }

        return spec;
    }

    public Optional<ActivityDTO> get(Long id) {
        return repository.findById(id).map(ActivityMapper::toDto);
    }
}


