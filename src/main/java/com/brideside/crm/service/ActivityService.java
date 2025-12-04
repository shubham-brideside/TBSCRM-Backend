package com.brideside.crm.service;

import com.brideside.crm.dto.ActivityDTO;
import com.brideside.crm.dto.ActivityDtos;
import com.brideside.crm.entity.Activity;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.mapper.ActivityMapper;
import com.brideside.crm.repository.ActivityRepository;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.JoinType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.util.ArrayList;
import java.util.Arrays;
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
                                  String serviceCategoryCodes,
                                  String organizationCategoryCodes,
                                  Pageable pageable) {
        Specification<Activity> spec = buildSpecification(
                personId, dateFrom, dateTo, assignedUser,
                organizationId, assignedUserId, category, status, done,
                serviceCategoryCodes, organizationCategoryCodes
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
                                        Boolean done,
                                        String serviceCategoryCodes,
                                        String organizationCategoryCodes) {
        Specification<Activity> baseSpec = buildSpecification(
                personId, dateFrom, dateTo, assignedUser,
                organizationId, assignedUserId, category, status, done,
                serviceCategoryCodes, organizationCategoryCodes
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
                                                       Boolean done,
                                                       String serviceCategoryCodes,
                                                       String organizationCategoryCodes) {
        Specification<Activity> spec = Specification.where(null);

        // Apply role-based scoping first (by IDs)
        // For Sales users: ONLY shows activities assigned to them or their Presales team members
        //                  (NOT activities for their organizations assigned to other users)
        // For Category Managers: ONLY shows activities assigned to Sales under them and Presales under those Sales
        //                        (NOT activities for organizations owned by those Sales assigned to other users)
        // For Presales users: ONLY shows activities assigned to themselves or their Sales manager
        //                     (NOT activities for organizations owned by their Sales manager assigned to other users)
        // For Admins: Uses organization OR assigned user scope
        ActivityScopeService.Scope scope = scopeService.resolveScope();
        Role.RoleName currentUserRole = scopeService.getCurrentUserRole();

        // Build OR condition: activity matches if organization or assigned user falls inside scope.
        // EXCEPTION: For SALES, CATEGORY_MANAGER, and PRESALES users, only use assigned user filtering (not organization filtering)
        boolean hasOrgIds = !scope.organizationIds().isEmpty();
        boolean hasOrgNames = !scope.organizationNames().isEmpty();
        boolean hasUserIds = !scope.assignedUserIds().isEmpty();
        boolean hasUserEmails = !scope.assignedUserEmails().isEmpty();

        // For SALES, CATEGORY_MANAGER, and PRESALES users, skip organization-based filtering - only show by assignment
        boolean restrictsByOrg = (currentUserRole != Role.RoleName.SALES && 
                                  currentUserRole != Role.RoleName.CATEGORY_MANAGER && 
                                  currentUserRole != Role.RoleName.PRESALES) && (hasOrgIds || hasOrgNames);
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
            // Apply lower bound on both `date` and `dueDate` so filters work for
            // task-style activities that only populate `dueDate` (Overdue tab).
            spec = spec.and((root, q, cb) -> {
                Predicate byDate = cb.greaterThanOrEqualTo(root.get("date"), from);
                Predicate byDueDate = cb.greaterThanOrEqualTo(root.get("dueDate"), from);
                return cb.or(byDate, byDueDate);
            });
        }
        if (dateTo != null && !dateTo.isBlank()) {
            String to = dateTo.trim();
            // Apply upper bound on both `date` and `dueDate` so "Overdue" and
            // other date range filters include activities using either field.
            spec = spec.and((root, q, cb) -> {
                Predicate byDate = cb.lessThanOrEqualTo(root.get("date"), to);
                Predicate byDueDate = cb.lessThanOrEqualTo(root.get("dueDate"), to);
                return cb.or(byDate, byDueDate);
            });
        }

        // Filter by service/organization category codes (PHOTOGRAPHY / MAKEUP / PLANNING_DECOR)
        List<String> categoryCodes = parseCategoryCodes(serviceCategoryCodes, organizationCategoryCodes);
        if (!categoryCodes.isEmpty()) {
            Specification<Activity> categorySpec = (root, q, cb) -> {
                List<Predicate> predicates = new ArrayList<>();

                // Match against Activity.serviceCategory (string codes)
                predicates.add(root.get("serviceCategory").in(categoryCodes));

                // Match against Organization.category (enum) when organizationRef is present
                List<Organization.OrganizationCategory> orgCategories = toOrganizationCategories(categoryCodes);
                if (!orgCategories.isEmpty()) {
                    predicates.add(
                            root.join("organizationRef", JoinType.LEFT)
                                    .get("category")
                                    .in(orgCategories)
                    );
                }

                if (predicates.isEmpty()) {
                    return cb.conjunction();
                }
                return orPredicates(cb, predicates);
            };
            spec = spec.and(categorySpec);
        }

        return spec;
    }

    /**
     * Parse one or two comma-separated category strings into normalized codes.
     */
    private List<String> parseCategoryCodes(String serviceCategoryCodes, String organizationCategoryCodes) {
        List<String> codes = new ArrayList<>();

        for (String source : Arrays.asList(serviceCategoryCodes, organizationCategoryCodes)) {
            if (source == null || source.isBlank()) continue;
            String[] parts = source.split(",");
            for (String raw : parts) {
                if (raw == null) continue;
                String normalized = raw.trim();
                if (!normalized.isEmpty()) {
                    // Normalize to upper-case code form, with underscores instead of spaces
                    String upper = normalized.toUpperCase().replace(' ', '_');
                    // Special-case common variants of Planning & Decor
                    if ("PLANNING_DECOR".equals(upper) || "PLANNING_AND_DECOR".equals(upper) || "PLANNING_&_DECOR".equals(upper)) {
                        upper = "PLANNING_AND_DECOR";
                    }
                    codes.add(upper);
                }
            }
        }
        return codes;
    }

    /**
     * Map normalized string codes to OrganizationCategory enums where possible.
     */
    private List<Organization.OrganizationCategory> toOrganizationCategories(List<String> codes) {
        List<Organization.OrganizationCategory> result = new ArrayList<>();
        for (String code : codes) {
            if (code == null || code.isBlank()) continue;
            switch (code) {
                case "PHOTOGRAPHY" -> result.add(Organization.OrganizationCategory.PHOTOGRAPHY);
                case "MAKEUP" -> result.add(Organization.OrganizationCategory.MAKEUP);
                case "PLANNING_DECOR", "PLANNING_AND_DECOR" ->
                        result.add(Organization.OrganizationCategory.PLANNING_AND_DECOR);
                default -> {
                    // Ignore unknown codes
                }
            }
        }
        return result;
    }

    public Optional<ActivityDTO> get(Long id) {
        return repository.findById(id).map(ActivityMapper::toDto);
    }
}


