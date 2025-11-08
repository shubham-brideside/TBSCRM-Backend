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

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
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
                                  String filter,
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
        // Handle filter parameter to set default dates when tab is selected but dates not provided
        // This fixes the issue where "Today" tab is selected by default but no dates are sent on initial load
        // Use a local variable for done status that can be modified
        Boolean doneFilter = done;
        
        // Check if dates are provided
        boolean datesProvided = (dateFrom != null && !dateFrom.isBlank()) || (dateTo != null && !dateTo.isBlank());
        
        // If no dates are provided, apply default filter logic
        if (!datesProvided) {
            LocalDate today = LocalDate.now();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
            
            if (filter != null && !filter.isBlank()) {
                String filterLower = filter.toLowerCase().trim();
                
                if ("today".equals(filterLower)) {
                    // Default to today's date
                    dateFrom = today.format(formatter);
                    dateTo = today.format(formatter);
                } else if ("overdue".equals(filterLower)) {
                    // Overdue: activities with date < today AND done = false
                    // Always filter by done=false for overdue, regardless of what frontend sends
                    dateTo = today.minusDays(1).format(formatter);
                    doneFilter = false; // Only show incomplete activities for overdue
                } else if ("tomorrow".equals(filterLower)) {
                    // Tomorrow's date
                    LocalDate tomorrow = today.plusDays(1);
                    dateFrom = tomorrow.format(formatter);
                    dateTo = tomorrow.format(formatter);
                } else if ("this week".equals(filterLower) || "thisweek".equals(filterLower)) {
                    // This week: from Monday to Sunday of current week
                    LocalDate monday = today.minusDays(today.getDayOfWeek().getValue() - 1);
                    LocalDate sunday = monday.plusDays(6);
                    dateFrom = monday.format(formatter);
                    dateTo = sunday.format(formatter);
                } else if ("next week".equals(filterLower) || "nextweek".equals(filterLower)) {
                    // Next week: from Monday to Sunday of next week
                    LocalDate nextMonday = today.plusDays(8 - today.getDayOfWeek().getValue());
                    LocalDate nextSunday = nextMonday.plusDays(6);
                    dateFrom = nextMonday.format(formatter);
                    dateTo = nextSunday.format(formatter);
                }
                // "All" and "To-do" filters don't need date filtering
            } else {
                // No filter specified and no dates provided - default to "Today" for initial page load
                // This handles the case where frontend has "Today" tab selected by default but doesn't send dates
                dateFrom = today.format(formatter);
                dateTo = today.format(formatter);
            }
        }
        
        // Apply done filter using the local variable (which may have been modified for overdue filter)
        if (doneFilter != null) {
            Boolean finalDone = doneFilter; // Make effectively final for lambda
            spec = spec.and((root, q, cb) -> cb.equal(root.get("done"), finalDone));
        }
        
        // Date filtering: Frontend sends dates in YYYY-MM-DD format, backend stores in dd/MM/yyyy format
        // Need to check both 'date' and 'dueDate' fields, preferring 'date' if available
        if (dateFrom != null && !dateFrom.isBlank() || dateTo != null && !dateTo.isBlank()) {
            spec = spec.and(createDateRangeSpecification(dateFrom, dateTo));
        }

        return repository.findAll(spec, pageable).map(ActivityMapper::toDto);
    }

    public Optional<ActivityDTO> get(Long id) {
        return repository.findById(id).map(ActivityMapper::toDto);
    }

    /**
     * Creates a date range specification that filters activities based on date/dueDate fields.
     * Frontend sends dates in YYYY-MM-DD format, backend stores in dd/MM/yyyy format.
     * Checks both 'date' and 'dueDate' fields, preferring 'date' if available.
     * Uses MySQL STR_TO_DATE function for proper date comparison.
     */
    private Specification<Activity> createDateRangeSpecification(String dateFrom, String dateTo) {
        return (root, query, cb) -> {
            // Parse incoming dates from YYYY-MM-DD format
            LocalDate fromDate = null;
            LocalDate toDate = null;
            
            DateTimeFormatter frontendFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd");
            
            try {
                if (dateFrom != null && !dateFrom.isBlank()) {
                    fromDate = LocalDate.parse(dateFrom.trim(), frontendFormat);
                }
                if (dateTo != null && !dateTo.isBlank()) {
                    toDate = LocalDate.parse(dateTo.trim(), frontendFormat);
                }
            } catch (DateTimeParseException e) {
                // If parsing fails, return a specification that matches nothing
                return cb.disjunction();
            }
            
            // If both dates are null, return a specification that matches everything
            if (fromDate == null && toDate == null) {
                return cb.conjunction();
            }
            
            // Use MySQL STR_TO_DATE function to convert dd/MM/yyyy string to DATE for proper comparison
            // Check both 'date' and 'dueDate' fields, preferring 'date' if available
            jakarta.persistence.criteria.Expression<java.sql.Date> dateField = 
                cb.function("STR_TO_DATE", java.sql.Date.class, 
                    root.get("date"), 
                    cb.literal("%d/%m/%Y"));
            
            jakarta.persistence.criteria.Expression<java.sql.Date> dueDateField = 
                cb.function("STR_TO_DATE", java.sql.Date.class, 
                    root.get("dueDate"), 
                    cb.literal("%d/%m/%Y"));
            
            // Convert LocalDate to java.sql.Date for comparison
            java.sql.Date fromSqlDate = fromDate != null ? java.sql.Date.valueOf(fromDate) : null;
            java.sql.Date toSqlDate = toDate != null ? java.sql.Date.valueOf(toDate) : null;
            
            // Handle exact date match (when dateFrom equals dateTo)
            if (fromDate != null && toDate != null && fromDate.equals(toDate)) {
                // Activity matches if its date (from date or dueDate) equals the exact date
                // Prefer 'date' field if available, otherwise use 'dueDate'
                return cb.or(
                    // Case 1: date field is not null and matches
                    cb.and(
                        cb.isNotNull(root.get("date")),
                        cb.equal(dateField, fromSqlDate)
                    ),
                    // Case 2: date is null but dueDate is not null and matches
                    cb.and(
                        cb.isNull(root.get("date")),
                        cb.isNotNull(root.get("dueDate")),
                        cb.equal(dueDateField, fromSqlDate)
                    )
                );
            }
            
            // Handle date range
            if (fromDate != null && toDate != null) {
                // Both dates provided - range filter (inclusive)
                // Prefer 'date' field if available, otherwise use 'dueDate'
                return cb.or(
                    // Case 1: date field is not null - check if it's in range
                    cb.and(
                        cb.isNotNull(root.get("date")),
                        cb.greaterThanOrEqualTo(dateField, fromSqlDate),
                        cb.lessThanOrEqualTo(dateField, toSqlDate)
                    ),
                    // Case 2: date is null but dueDate is not null - check dueDate
                    cb.and(
                        cb.isNull(root.get("date")),
                        cb.isNotNull(root.get("dueDate")),
                        cb.greaterThanOrEqualTo(dueDateField, fromSqlDate),
                        cb.lessThanOrEqualTo(dueDateField, toSqlDate)
                    )
                );
            } else if (fromDate != null) {
                // Only fromDate provided
                return cb.or(
                    cb.and(
                        cb.isNotNull(root.get("date")),
                        cb.greaterThanOrEqualTo(dateField, fromSqlDate)
                    ),
                    cb.and(
                        cb.isNull(root.get("date")),
                        cb.isNotNull(root.get("dueDate")),
                        cb.greaterThanOrEqualTo(dueDateField, fromSqlDate)
                    )
                );
            } else if (toDate != null) {
                // Only toDate provided (e.g., for "Overdue" tab)
                return cb.or(
                    cb.and(
                        cb.isNotNull(root.get("date")),
                        cb.lessThanOrEqualTo(dateField, toSqlDate)
                    ),
                    cb.and(
                        cb.isNull(root.get("date")),
                        cb.isNotNull(root.get("dueDate")),
                        cb.lessThanOrEqualTo(dueDateField, toSqlDate)
                    )
                );
            }
            
            return cb.conjunction();
        };
    }
}


