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
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.PageImpl;

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
                                  List<Long> organizationIds,
                                  List<Long> assignedUserIds,
                                  String category,
                                  String status,
                                  Boolean done,
                                  String serviceCategoryCodes,
                                  String organizationCategoryCodes,
                                  Long dealId,
                                  Pageable pageable) {
        // Build specification WITHOUT date filters (we'll filter dates in memory)
        Specification<Activity> spec = buildSpecification(
                personId, null, null, assignedUser, // Pass null for dateFrom/dateTo
                organizationIds, assignedUserIds, category, status, done,
                serviceCategoryCodes, organizationCategoryCodes, dealId
        );

        // Parse date filters for in-memory filtering
        LocalDate fromDate = null;
        LocalDate toDate = null;
        if (dateFrom != null && !dateFrom.isBlank()) {
            try {
                fromDate = LocalDate.parse(dateFrom.trim(), DateTimeFormatter.ofPattern("yyyy-MM-dd"));
            } catch (Exception e) {
                // Invalid date format, ignore
            }
        }
        if (dateTo != null && !dateTo.isBlank()) {
            try {
                toDate = LocalDate.parse(dateTo.trim(), DateTimeFormatter.ofPattern("yyyy-MM-dd"));
            } catch (Exception e) {
                // Invalid date format, ignore
            }
        }
        
        final LocalDate finalFromDate = fromDate;
        final LocalDate finalToDate = toDate;
        final boolean hasDateFilter = finalFromDate != null || finalToDate != null;

        Page<Activity> activities;
        
        if (hasDateFilter) {
            // For date filtering, we need to:
            // 1. Load ALL activities matching non-date filters (to get accurate count)
            // 2. Filter by date in memory
            // 3. Deduplicate by ID
            // 4. Sort
            // 5. Apply pagination
            
            // Load all activities matching non-date filters (without pagination)
            List<Activity> allActivities = repository.findAll(spec);
            
            // Force load organizationRef and owner to avoid lazy loading issues
            allActivities.forEach(activity -> {
                if (activity.getOrganizationRef() != null) {
                    activity.getOrganizationRef().getName();
                    if (activity.getOrganizationRef().getOwner() != null) {
                        activity.getOrganizationRef().getOwner().getEmail();
                    }
                }
            });
            
            // Filter by date in memory
            List<Activity> filteredActivities = allActivities.stream()
                .filter(activity -> matchesDateFilter(activity, finalFromDate, finalToDate))
                .collect(java.util.stream.Collectors.toList());
            
            // Deduplicate by ID (in case of any duplicates from joins)
            java.util.Map<Long, Activity> uniqueActivities = new java.util.LinkedHashMap<>();
            for (Activity activity : filteredActivities) {
                if (activity.getId() != null && !uniqueActivities.containsKey(activity.getId())) {
                    uniqueActivities.put(activity.getId(), activity);
                }
            }
            List<Activity> deduplicatedActivities = new ArrayList<>(uniqueActivities.values());
            
            // Apply sorting
            if (pageable.getSort().isSorted()) {
                pageable.getSort().forEach(order -> {
                    String property = order.getProperty();
                    boolean ascending = order.isAscending();
                    
                    deduplicatedActivities.sort((a1, a2) -> {
                        int comparison = 0;
                        try {
                            switch (property) {
                                case "dueDate":
                                    comparison = compareDateStrings(a1.getDueDate(), a2.getDueDate());
                                    break;
                                case "date":
                                    comparison = compareDateStrings(a1.getDate(), a2.getDate());
                                    break;
                                case "dateTime":
                                    comparison = compareDateStrings(a1.getDateTime(), a2.getDateTime());
                                    break;
                                case "id":
                                    comparison = Long.compare(
                                        a1.getId() != null ? a1.getId() : 0L,
                                        a2.getId() != null ? a2.getId() : 0L
                                    );
                                    break;
                                case "subject":
                                    comparison = (a1.getSubject() != null ? a1.getSubject() : "")
                                        .compareToIgnoreCase(a2.getSubject() != null ? a2.getSubject() : "");
                                    break;
                                default:
                                    comparison = 0;
                            }
                        } catch (Exception e) {
                            comparison = 0;
                        }
                        return ascending ? comparison : -comparison;
                    });
                });
            } else {
                // Default sort: by dueDate desc, then id desc
                deduplicatedActivities.sort((a1, a2) -> {
                    int dateComparison = compareDateStrings(a1.getDueDate(), a2.getDueDate());
                    if (dateComparison != 0) return -dateComparison; // desc
                    return Long.compare(
                        a2.getId() != null ? a2.getId() : 0L,
                        a1.getId() != null ? a1.getId() : 0L
                    ); // desc
                });
            }
            
            // Apply pagination
            int page = pageable.getPageNumber();
            int size = pageable.getPageSize();
            int totalElements = deduplicatedActivities.size();
            int start = page * size;
            int end = Math.min(start + size, totalElements);
            
            List<Activity> paginatedActivities = start < totalElements 
                ? deduplicatedActivities.subList(start, end)
                : new ArrayList<>();
            
            // Create a new Page with filtered, deduplicated, sorted, and paginated results
            // PageImpl automatically calculates totalPages from totalElements and size
            activities = new PageImpl<>(
                paginatedActivities,
                pageable,
                totalElements
            );
        } else {
            // No date filter - still deduplicate and paginate to ensure totalElements/totalPages are accurate
            List<Activity> allActivities = repository.findAll(spec);

            // Force load organizationRef and owner to avoid lazy loading issues
            allActivities.forEach(activity -> {
                if (activity.getOrganizationRef() != null) {
                    activity.getOrganizationRef().getName();
                    if (activity.getOrganizationRef().getOwner() != null) {
                        activity.getOrganizationRef().getOwner().getEmail();
                    }
                }
            });

            // Deduplicate by ID
            java.util.Map<Long, Activity> uniqueActivities = new java.util.LinkedHashMap<>();
            for (Activity activity : allActivities) {
                if (activity.getId() != null && !uniqueActivities.containsKey(activity.getId())) {
                    uniqueActivities.put(activity.getId(), activity);
                }
            }
            List<Activity> deduplicatedActivities = new ArrayList<>(uniqueActivities.values());

            // Apply sorting
            if (pageable.getSort().isSorted()) {
                pageable.getSort().forEach(order -> {
                    String property = order.getProperty();
                    boolean ascending = order.isAscending();

                    deduplicatedActivities.sort((a1, a2) -> {
                        int comparison = 0;
                        try {
                            switch (property) {
                                case "dueDate":
                                    comparison = compareDateStrings(a1.getDueDate(), a2.getDueDate());
                                    break;
                                case "date":
                                    comparison = compareDateStrings(a1.getDate(), a2.getDate());
                                    break;
                                case "dateTime":
                                    comparison = compareDateStrings(a1.getDateTime(), a2.getDateTime());
                                    break;
                                case "id":
                                    comparison = Long.compare(
                                        a1.getId() != null ? a1.getId() : 0L,
                                        a2.getId() != null ? a2.getId() : 0L
                                    );
                                    break;
                                case "subject":
                                    comparison = (a1.getSubject() != null ? a1.getSubject() : "")
                                        .compareToIgnoreCase(a2.getSubject() != null ? a2.getSubject() : "");
                                    break;
                                default:
                                    comparison = 0;
                            }
                        } catch (Exception e) {
                            comparison = 0;
                        }
                        return ascending ? comparison : -comparison;
                    });
                });
            } else {
                // Default sort: by dueDate desc, then id desc
                deduplicatedActivities.sort((a1, a2) -> {
                    int dateComparison = compareDateStrings(a1.getDueDate(), a2.getDueDate());
                    if (dateComparison != 0) return -dateComparison; // desc
                    return Long.compare(
                        a2.getId() != null ? a2.getId() : 0L,
                        a1.getId() != null ? a1.getId() : 0L
                    ); // desc
                });
            }

            // Apply pagination
            int page = pageable.getPageNumber();
            int size = pageable.getPageSize();
            int totalElements = deduplicatedActivities.size();
            int start = page * size;
            int end = Math.min(start + size, totalElements);

            List<Activity> paginatedActivities = start < totalElements
                ? deduplicatedActivities.subList(start, end)
                : new ArrayList<>();

            // Create a new Page with deduplicated, sorted, paginated results
            activities = new PageImpl<>(
                paginatedActivities,
                pageable,
                totalElements
            );
        }

        return activities.map(ActivityMapper::toDto);
    }
    
    /**
     * Compare two date strings (handles DD/MM/YYYY and ISO formats)
     * Returns: negative if date1 < date2, positive if date1 > date2, 0 if equal
     */
    private int compareDateStrings(String date1, String date2) {
        if (date1 == null && date2 == null) return 0;
        if (date1 == null) return 1; // null dates go to end
        if (date2 == null) return -1;
        
        try {
            LocalDate localDate1 = parseDateString(date1);
            LocalDate localDate2 = parseDateString(date2);
            if (localDate1 == null && localDate2 == null) return 0;
            if (localDate1 == null) return 1;
            if (localDate2 == null) return -1;
            return localDate1.compareTo(localDate2);
        } catch (Exception e) {
            // Fallback to string comparison
            return date1.compareTo(date2);
        }
    }
    
    /**
     * Parse a date string in various formats (DD/MM/YYYY, ISO, etc.)
     */
    private LocalDate parseDateString(String dateStr) {
        if (dateStr == null || dateStr.trim().isEmpty()) {
            return null;
        }
        
        String trimmed = dateStr.trim();
        DateTimeFormatter ddMMyyyyFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        DateTimeFormatter isoFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss");
        DateTimeFormatter isoDateOnly = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        
        try {
            // Try DD/MM/YYYY format first
            return LocalDate.parse(trimmed, ddMMyyyyFormatter);
        } catch (Exception e1) {
            try {
                // Try ISO date-only format
                if (trimmed.length() == 10) {
                    return LocalDate.parse(trimmed, isoDateOnly);
                }
                // Try ISO with time
                if (trimmed.contains("T")) {
                    return LocalDateTime.parse(trimmed, isoFormatter).toLocalDate();
                }
            } catch (Exception e2) {
                // Could not parse
            }
        }
        return null;
    }
    
    /**
     * Check if an activity matches the date filter by checking all date fields:
     * date (DD/MM/YYYY), dueDate (DD/MM/YYYY), and dateTime (ISO format)
     */
    private boolean matchesDateFilter(Activity activity, LocalDate fromDate, LocalDate toDate) {
        if (fromDate == null && toDate == null) {
            return true; // No date filter
        }
        
        DateTimeFormatter ddMMyyyyFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        DateTimeFormatter isoFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss");
        DateTimeFormatter isoFormatterWithZone = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssXXX");
        DateTimeFormatter isoDateOnly = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        
        // Check date field (DD/MM/YYYY format)
        if (activity.getDate() != null && !activity.getDate().trim().isEmpty()) {
            try {
                LocalDate activityDate = LocalDate.parse(activity.getDate().trim(), ddMMyyyyFormatter);
                if (isDateInRange(activityDate, fromDate, toDate)) {
                    return true;
                }
            } catch (Exception e) {
                // Invalid date format, skip this field
            }
        }
        
        // Check dueDate field (DD/MM/YYYY format)
        if (activity.getDueDate() != null && !activity.getDueDate().trim().isEmpty()) {
            try {
                LocalDate activityDueDate = LocalDate.parse(activity.getDueDate().trim(), ddMMyyyyFormatter);
                if (isDateInRange(activityDueDate, fromDate, toDate)) {
                    return true;
                }
            } catch (Exception e) {
                // Invalid date format, skip this field
            }
        }
        
        // Check dateTime field (ISO format or other formats)
        if (activity.getDateTime() != null && !activity.getDateTime().trim().isEmpty()) {
            try {
                String dateTimeStr = activity.getDateTime().trim();
                LocalDate activityDateTime = null;
                
                // Try different date formats
                if (dateTimeStr.contains("T")) {
                    // ISO format with time
                    try {
                        // Check if it has timezone indicator (+, Z, or - after position 10)
                        boolean hasTimezone = dateTimeStr.contains("+") || dateTimeStr.contains("Z") || 
                            (dateTimeStr.length() > 10 && dateTimeStr.charAt(10) == '-' && dateTimeStr.length() > 19);
                        if (hasTimezone) {
                            // Has timezone
                            activityDateTime = OffsetDateTime.parse(dateTimeStr, isoFormatterWithZone).toLocalDate();
                        } else {
                            // No timezone
                            activityDateTime = LocalDateTime.parse(dateTimeStr, isoFormatter).toLocalDate();
                        }
                    } catch (Exception e) {
                        // Try parsing as LocalDateTime without seconds
                        try {
                            activityDateTime = LocalDateTime.parse(dateTimeStr, DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm")).toLocalDate();
                        } catch (Exception e2) {
                            // Try extracting just the date portion
                            if (dateTimeStr.length() >= 10) {
                                activityDateTime = LocalDate.parse(dateTimeStr.substring(0, 10), isoDateOnly);
                            }
                        }
                    }
                } else if (dateTimeStr.length() == 10) {
                    // Just date portion
                    activityDateTime = LocalDate.parse(dateTimeStr, isoDateOnly);
                }
                
                if (activityDateTime != null && isDateInRange(activityDateTime, fromDate, toDate)) {
                    return true;
                }
            } catch (Exception e) {
                // Invalid date format, skip this field
            }
        }
        
        // If no date fields match, exclude the activity
        return false;
    }
    
    /**
     * Check if a date falls within the specified range (inclusive)
     */
    private boolean isDateInRange(LocalDate date, LocalDate fromDate, LocalDate toDate) {
        if (date == null) {
            return false;
        }
        if (fromDate != null && date.isBefore(fromDate)) {
            return false;
        }
        if (toDate != null && date.isAfter(toDate)) {
            return false;
        }
        return true;
    }

    /**
     * Compute summary counts for dashboard cards using the same scope and filters as list().
     */
    public ActivityDtos.Summary summary(Long personId,
                                        String dateFrom,
                                        String dateTo,
                                        String assignedUser,
                                        List<Long> organizationIds,
                                        List<Long> assignedUserIds,
                                        String category,
                                        String status,
                                        Boolean done,
                                        String serviceCategoryCodes,
                                        String organizationCategoryCodes) {
        ActivityDtos.Summary s = new ActivityDtos.Summary();
        
        // Build base spec without category filter (we'll add category filter per count)
        // This ensures each count uses the correct category filter regardless of request parameters
        Specification<Activity> baseSpecWithoutCategory = buildSpecification(
                personId, dateFrom, dateTo, assignedUser,
                organizationIds, assignedUserIds, null, status, done, // category = null (we'll filter by category per count)
                serviceCategoryCodes, organizationCategoryCodes, null
        );
        
        // Total Activities: category = 'ACTIVITY' (always filter by ACTIVITY category)
        // SELECT * FROM activities WHERE category = 'ACTIVITY'
        Specification<Activity> activityTotalSpec = baseSpecWithoutCategory.and((root, q, cb) ->
                cb.equal(root.get("category"), Activity.ActivityCategory.ACTIVITY));
        long totalCount = repository.count(activityTotalSpec);
        s.setActivityTotalCount(totalCount);

        // Pending: category = 'ACTIVITY' and done = false
        // SELECT * FROM activities WHERE category = 'ACTIVITY' AND done = 0
        Specification<Activity> pendingSpec = baseSpecWithoutCategory.and((root, q, cb) ->
                cb.and(
                    cb.equal(root.get("category"), Activity.ActivityCategory.ACTIVITY),
                    cb.isFalse(root.get("done"))
                ));
        long pendingCount = repository.count(pendingSpec);
        s.setActivityPendingCount(pendingCount);

        // Completed: category = 'ACTIVITY' and done = true
        // SELECT * FROM activities WHERE category = 'ACTIVITY' AND done = 1
        Specification<Activity> completedSpec = baseSpecWithoutCategory.and((root, q, cb) ->
                cb.and(
                    cb.equal(root.get("category"), Activity.ActivityCategory.ACTIVITY),
                    cb.isTrue(root.get("done"))
                ));
        long completedCount = repository.count(completedSpec);
        s.setActivityCompletedCount(completedCount);

        // Call Assigned: category = 'CALL' (always filter by CALL category, regardless of done status)
        // SELECT * FROM activities WHERE category = 'CALL'
        Specification<Activity> callSpec = baseSpecWithoutCategory.and((root, q, cb) ->
                cb.equal(root.get("category"), Activity.ActivityCategory.CALL));
        s.setCallAssignedCount(repository.count(callSpec));

        // Meeting Assigned: category = 'MEETING_SCHEDULER' (always filter by MEETING_SCHEDULER category, regardless of done status)
        Specification<Activity> meetingSpec = baseSpecWithoutCategory.and((root, q, cb) ->
                cb.equal(root.get("category"), Activity.ActivityCategory.MEETING_SCHEDULER));
        s.setMeetingAssignedCount(repository.count(meetingSpec));

        // Optional: Call Taken - category = 'CALL' and done = true
        Specification<Activity> callTakenSpec = baseSpecWithoutCategory.and((root, q, cb) ->
                cb.and(
                    cb.equal(root.get("category"), Activity.ActivityCategory.CALL),
                    cb.isTrue(root.get("done"))
                ));
        s.setCallTakenCount(repository.count(callTakenSpec));

        // Optional: Meeting Done - category = 'MEETING_SCHEDULER' and done = true
        Specification<Activity> meetingDoneSpec = baseSpecWithoutCategory.and((root, q, cb) ->
                cb.and(
                    cb.equal(root.get("category"), Activity.ActivityCategory.MEETING_SCHEDULER),
                    cb.isTrue(root.get("done"))
                ));
        s.setMeetingDoneCount(repository.count(meetingDoneSpec));

        // Optional: Overdue - category = 'ACTIVITY' and not completed and dueDate is in the past
        // Note: dueDate is stored as string in "dd/MM/yyyy" format
        // We need to compare dates properly - load activities and filter in memory
        // for accurate date comparison since string comparison doesn't work for dates
        Specification<Activity> overdueBaseSpec = baseSpecWithoutCategory.and((root, q, cb) -> 
            cb.and(
                cb.equal(root.get("category"), Activity.ActivityCategory.ACTIVITY),
                cb.isFalse(root.get("done")),
                cb.isNotNull(root.get("dueDate"))
            ));
        try {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
            LocalDate today = LocalDate.now();
            long overdueCount = repository.findAll(overdueBaseSpec).stream()
                .filter(a -> {
                    if (a.getDueDate() == null || a.getDueDate().trim().isEmpty()) {
                        return false;
                    }
                    try {
                        LocalDate dueDate = LocalDate.parse(a.getDueDate(), formatter);
                        return dueDate.isBefore(today);
                    } catch (Exception e) {
                        // If date parsing fails, skip this activity
                        return false;
                    }
                })
                .count();
            s.setOverdueCount(overdueCount);
        } catch (Exception e) {
            // If calculation fails, set to null (optional field)
            s.setOverdueCount(null);
        }

        // Optional: Total Call Duration - sum of durationMinutes for completed CALL activities
        // This requires a custom query to sum the durationMinutes field
        try {
            Long totalDuration = repository.findAll(callTakenSpec).stream()
                .filter(a -> a.getDurationMinutes() != null)
                .mapToLong(Activity::getDurationMinutes)
                .sum();
            s.setTotalCallDurationMinutes(totalDuration);
        } catch (Exception e) {
            // If calculation fails, set to null (optional field)
            s.setTotalCallDurationMinutes(null);
        }

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
                                                       List<Long> organizationIds,
                                                       List<Long> assignedUserIds,
                                                       String category,
                                                       String status,
                                                       Boolean done,
                                                       String serviceCategoryCodes,
                                                       String organizationCategoryCodes,
                                                       Long dealId) {
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
        boolean hasUserNames = !scope.assignedUserNames().isEmpty();

        // For SALES, CATEGORY_MANAGER, and PRESALES users, skip organization-based filtering - only show by assignment
        boolean restrictsByOrg = (currentUserRole != Role.RoleName.SALES && 
                                  currentUserRole != Role.RoleName.CATEGORY_MANAGER && 
                                  currentUserRole != Role.RoleName.PRESALES) && (hasOrgIds || hasOrgNames);
        boolean restrictsByUser = hasUserIds || hasUserEmails || hasUserNames;

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
                    if (hasUserNames) {
                        // Also check assignedUser against user display names (firstName + lastName)
                        userPredicates.add(
                                cb.lower(root.get("assignedUser")).in(scope.assignedUserNames()));
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
        // Support both single value and list of values for organizationId and assignedUserId
        if (organizationIds != null && !organizationIds.isEmpty()) {
            spec = spec.and((root, q, cb) -> root.get("organizationId").in(organizationIds));
        }
        if (assignedUserIds != null && !assignedUserIds.isEmpty()) {
            spec = spec.and((root, q, cb) -> root.get("assignedUserId").in(assignedUserIds));
        }
        
        // Filter by dealId if provided
        if (dealId != null) {
            spec = spec.and((root, q, cb) -> cb.equal(root.get("dealId"), dealId));
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
        // Date filtering is now handled in the list() method after loading activities
        // This allows us to properly parse and compare dates in different formats
        // (Frontend sends YYYY-MM-DD, backend stores DD/MM/YYYY in date/dueDate fields)

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


