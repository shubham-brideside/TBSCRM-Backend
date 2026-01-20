package com.brideside.crm.service;

import com.brideside.crm.dto.PersonDTO;
import com.brideside.crm.dto.PersonSummaryDTO;
import com.brideside.crm.entity.Category;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Person;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.DuplicatePersonException;
import com.brideside.crm.mapper.PersonMapper;
import com.brideside.crm.repository.ActivityRepository;
import com.brideside.crm.repository.CategoryRepository;
import com.brideside.crm.repository.DealRepository;
import com.brideside.crm.repository.LabelRepository;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.repository.PersonRepository;
import com.brideside.crm.repository.PersonSpecifications;
import com.brideside.crm.repository.UserRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import java.time.LocalDate;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;

@Service
@Transactional
public class PersonService {

    private final PersonRepository repository;
    private final OrganizationRepository organizationRepository;
    private final UserRepository userRepository;
    private final CategoryRepository categoryRepository;
    private final LabelRepository labelRepository;
    private final DealRepository dealRepository;
    private final ActivityRepository activityRepository;

    public PersonService(PersonRepository repository,
                         OrganizationRepository organizationRepository,
                         UserRepository userRepository,
                         CategoryRepository categoryRepository,
                         LabelRepository labelRepository,
                         DealRepository dealRepository,
                         ActivityRepository activityRepository) {
        this.repository = repository;
        this.organizationRepository = organizationRepository;
        this.userRepository = userRepository;
        this.categoryRepository = categoryRepository;
        this.labelRepository = labelRepository;
        this.dealRepository = dealRepository;
        this.activityRepository = activityRepository;
    }

    public Page<PersonDTO> list(String q,
                                List<Person.PersonLabel> labels,
                                List<Long> organizationIds,
                                List<Long> ownerIds,
                                List<Long> categoryIds,
                                com.brideside.crm.entity.DealSource source,
                                com.brideside.crm.entity.DealSource dealSource,
                                LocalDate leadDateFrom,
                                LocalDate leadDateTo,
                                Pageable pageable) {

        Specification<Person> spec = Specification.where(PersonSpecifications.notDeleted())
                .and(PersonSpecifications.search(q))
                .and(PersonSpecifications.hasLabels(labels))
                .and(PersonSpecifications.hasOrganizations(organizationIds))
                .and(PersonSpecifications.hasOwners(ownerIds))
                .and(PersonSpecifications.hasCategories(categoryIds))
                .and(PersonSpecifications.hasSource(source))
                .and(PersonSpecifications.hasDealSource(dealSource))
                .and(PersonSpecifications.leadDateBetween(leadDateFrom, leadDateTo));

        return repository.findAll(spec, pageable).map(PersonMapper::toDto);
    }

    public PersonDTO get(Long id) {
        Person person = repository.findById(id)
                .orElseThrow(() -> new NoSuchElementException("Person not found"));
        
        // Check if person is soft-deleted
        if (Boolean.TRUE.equals(person.getIsDeleted())) {
            throw new NoSuchElementException("Person not found");
        }
        
        return PersonMapper.toDto(person);
    }

    /**
     * Check for duplicate persons by phone number or Instagram ID
     * @param phone Phone number to check (optional)
     * @param instagramId Instagram ID to check (optional)
     * @param excludeId Person ID to exclude from results (useful for edit scenarios)
     * @return List of matching persons (empty if no duplicates found)
     */
    public List<PersonDTO> checkDuplicates(String phone, String instagramId, Long excludeId) {
        Specification<Person> spec = Specification.where(PersonSpecifications.notDeleted());

        // Build search criteria based on what's provided
        // If only phone is provided, search only by phone
        // If only Instagram ID is provided, search only by Instagram ID
        // If both are provided, search by phone OR Instagram ID
        Specification<Person> phoneOrInstagramSpec = null;
        boolean hasPhone = StringUtils.hasText(phone);
        boolean hasInstagramId = StringUtils.hasText(instagramId);
        
        if (hasPhone && hasInstagramId) {
            // Both provided: search by phone OR Instagram ID
            phoneOrInstagramSpec = PersonSpecifications.hasPhone(phone)
                    .or(PersonSpecifications.hasInstagramId(instagramId));
        } else if (hasPhone) {
            // Only phone provided: search only by phone
            phoneOrInstagramSpec = PersonSpecifications.hasPhone(phone);
        } else if (hasInstagramId) {
            // Only Instagram ID provided: search only by Instagram ID
            phoneOrInstagramSpec = PersonSpecifications.hasInstagramId(instagramId);
        } else {
            // Neither provided: return empty list
            return List.of();
        }

        spec = spec.and(phoneOrInstagramSpec);

        // Exclude specific person ID if provided (CRITICAL: must be applied)
        if (excludeId != null) {
            spec = spec.and(PersonSpecifications.excludeId(excludeId));
        }

        List<Person> duplicates = repository.findAll(spec);
        
        // Filter results to ensure exact trimmed match (handles database whitespace differences)
        // Also add defensive check to ensure excludeId is never in results
        String trimmedPhone = phone != null ? phone.trim().toLowerCase() : null;
        String trimmedInstagramId = instagramId != null ? instagramId.trim().toLowerCase() : null;
        
        return duplicates.stream()
                .filter(p -> {
                    // Defensive check: never include the excluded person, even if it somehow got through
                    if (excludeId != null && p.getId().equals(excludeId)) {
                        return false;
                    }
                    
                    boolean phoneMatch = true;
                    boolean instagramMatch = true;
                    
                    if (trimmedPhone != null && !trimmedPhone.isEmpty()) {
                        String personPhone = p.getPhone() != null ? p.getPhone().trim().toLowerCase() : "";
                        phoneMatch = personPhone.equals(trimmedPhone);
                    }
                    
                    if (trimmedInstagramId != null && !trimmedInstagramId.isEmpty()) {
                        String personInstagramId = p.getInstagramId() != null ? p.getInstagramId().trim().toLowerCase() : "";
                        instagramMatch = personInstagramId.equals(trimmedInstagramId);
                    }
                    
                    return phoneMatch || instagramMatch;
                })
                .map(PersonMapper::toDto)
                .collect(Collectors.toList());
    }

    public PersonSummaryDTO getSummary(Long id) {
        Person person = repository.findById(id)
                .orElseThrow(() -> new NoSuchElementException("Person not found"));
        
        // Check if person is soft-deleted
        if (Boolean.TRUE.equals(person.getIsDeleted())) {
            throw new NoSuchElementException("Person not found");
        }
        
        PersonSummaryDTO summary = new PersonSummaryDTO();
        summary.setPerson(PersonMapper.toDto(person));
        summary.setDealsCount(0L);
        return summary;
    }

    public PersonDTO create(PersonDTO dto) {
        return create(dto, false);
    }

    public PersonDTO create(PersonDTO dto, boolean skipDuplicateCheck) {
        // Check for duplicates before creating (unless skipDuplicateCheck is true)
        // skipDuplicateCheck should only be used when we know a merge will happen immediately after
        if (!skipDuplicateCheck) {
            List<PersonDTO> duplicates = checkDuplicates(dto.getPhone(), dto.getInstagramId(), null);
            if (!duplicates.isEmpty()) {
                PersonDTO duplicatePerson = duplicates.get(0);
                throw new DuplicatePersonException(
                    "A person with this phone number or Instagram ID already exists",
                    duplicatePerson
                );
            }
        }
        
        Person entity = new Person();
        apply(dto, entity);
        // Handle custom label (single label from labels table)
        if (dto.getLabelId() != null) {
            // Use repository directly to get a managed entity in the same transaction
            com.brideside.crm.entity.Label label = labelRepository.findById(dto.getLabelId())
                    .orElseThrow(() -> new BadRequestException("Label not found with id: " + dto.getLabelId()));
            
            // Verify label is not deleted
            if (label.getIsDeleted() != null && label.getIsDeleted()) {
                throw new BadRequestException("Label with id " + dto.getLabelId() + " has been deleted");
            }
            
            entity.setLabel(label);
        }
        Person saved = repository.save(entity);
        return PersonMapper.toDto(saved);
    }

    public PersonDTO update(Long id, PersonDTO dto) {
        return update(id, dto, false);
    }

    public PersonDTO update(Long id, PersonDTO dto, boolean skipDuplicateCheck) {
        Person entity = repository.findById(id)
                .orElseThrow(() -> new NoSuchElementException("Person not found"));
        
        // Check if person is soft-deleted
        if (Boolean.TRUE.equals(entity.getIsDeleted())) {
            throw new NoSuchElementException("Person not found");
        }
        
        // Check if phone or Instagram ID is being changed and if it matches another person
        // (unless skipDuplicateCheck is true, which should only be used when merge will happen immediately after)
        // 
        // Edge Case Handling:
        // - Person A has Instagram ID only, Person B has phone only
        // - When Person A adds phone number, check if it matches Person B's phone
        // - When Person B adds Instagram ID, check if it matches Person A's Instagram ID
        // - In both cases, we need to detect the duplicate and prompt for merge
        if (!skipDuplicateCheck) {
            String currentPhone = entity.getPhone() != null ? entity.getPhone().trim() : "";
            String newPhone = dto.getPhone() != null ? dto.getPhone().trim() : "";
            boolean phoneChanged = StringUtils.hasText(newPhone) && !newPhone.equals(currentPhone);
            boolean phoneAdded = !StringUtils.hasText(currentPhone) && StringUtils.hasText(newPhone);
            
            String currentInstagramId = entity.getInstagramId() != null ? entity.getInstagramId().trim() : "";
            String newInstagramId = dto.getInstagramId() != null ? dto.getInstagramId().trim() : "";
            boolean instagramIdChanged = StringUtils.hasText(newInstagramId) && !newInstagramId.equals(currentInstagramId);
            boolean instagramIdAdded = !StringUtils.hasText(currentInstagramId) && StringUtils.hasText(newInstagramId);
            
            // Check for duplicates if phone or Instagram ID is being changed
            // IMPORTANT: Only check by the field that actually changed to avoid false positives
            // This handles the edge case where:
            // 1. Person with Instagram ID adds phone → should find person with that phone (only)
            // 2. Person with phone adds Instagram ID → should find person with that Instagram ID (only)
            // 3. Person changes phone/Instagram ID → should find person with new value (only)
            if (phoneChanged || phoneAdded) {
                // Only check by phone (the field that changed)
                // Pass null for instagramId to ensure we only search by phone
                List<PersonDTO> duplicates = checkDuplicates(newPhone, null, id);
                if (!duplicates.isEmpty()) {
                    PersonDTO duplicatePerson = duplicates.get(0);
                    throw new DuplicatePersonException(
                        "A person with this phone number or Instagram ID already exists",
                        duplicatePerson
                    );
                }
            }
            
            if (instagramIdChanged || instagramIdAdded) {
                // Only check by Instagram ID (the field that changed)
                // Pass null for phone to ensure we only search by Instagram ID
                List<PersonDTO> duplicates = checkDuplicates(null, newInstagramId, id);
                if (!duplicates.isEmpty()) {
                    PersonDTO duplicatePerson = duplicates.get(0);
                    throw new DuplicatePersonException(
                        "A person with this phone number or Instagram ID already exists",
                        duplicatePerson
                    );
                }
            }
        }
        
        apply(dto, entity);
        // Handle custom label (single label from labels table)
        if (dto.getLabelId() != null) {
            // Use repository directly to get a managed entity in the same transaction
            com.brideside.crm.entity.Label label = labelRepository.findById(dto.getLabelId())
                    .orElseThrow(() -> new BadRequestException("Label not found with id: " + dto.getLabelId()));
            
            // Verify label is not deleted
            if (label.getIsDeleted() != null && label.getIsDeleted()) {
                throw new BadRequestException("Label with id " + dto.getLabelId() + " has been deleted");
            }
            
            entity.setLabel(label);
        }
        Person saved = repository.save(entity);
        return PersonMapper.toDto(saved);
    }

    public void delete(Long id) {
        Person person = repository.findById(id)
                .orElseThrow(() -> new NoSuchElementException("Person not found"));
        
        // Check if person is already soft-deleted
        if (Boolean.TRUE.equals(person.getIsDeleted())) {
            throw new NoSuchElementException("Person not found");
        }
        
        // Soft delete: set is_deleted to true instead of hard delete
        person.setIsDeleted(Boolean.TRUE);
        repository.save(person);
    }

    public long bulkDelete(List<Long> ids) {
        List<Person> all = repository.findAllById(ids);
        // Filter out already deleted persons
        List<Person> toDelete = all.stream()
                .filter(p -> !Boolean.TRUE.equals(p.getIsDeleted()))
                .toList();
        
        // Soft delete: set is_deleted to true for all persons
        toDelete.forEach(p -> p.setIsDeleted(Boolean.TRUE));
        repository.saveAll(toDelete);
        
        return toDelete.size();
    }

    public PersonDTO merge(Long targetId, List<Long> duplicateIds) {
        // Validate input
        if (duplicateIds == null || duplicateIds.isEmpty()) {
            return get(targetId);
        }

        // Remove duplicates and target ID from the list
        List<Long> uniqueDuplicateIds = duplicateIds.stream()
                .distinct()
                .filter(id -> !id.equals(targetId))
                .collect(Collectors.toList());

        if (uniqueDuplicateIds.isEmpty()) {
            return get(targetId);
        }

        // Validate target person exists and is not deleted
        Person target = repository.findById(targetId)
                .orElseThrow(() -> new BadRequestException("Target person not found with id: " + targetId));
        
        if (Boolean.TRUE.equals(target.getIsDeleted())) {
            throw new BadRequestException("Target person not found with id: " + targetId);
        }

        // Validate all duplicate persons exist and are not deleted
        List<Person> sources = repository.findAllById(uniqueDuplicateIds);
        if (sources.size() != uniqueDuplicateIds.size()) {
            List<Long> foundIds = sources.stream().map(Person::getId).collect(Collectors.toList());
            List<Long> missingIds = uniqueDuplicateIds.stream()
                    .filter(id -> !foundIds.contains(id))
                    .collect(Collectors.toList());
            throw new BadRequestException("One or more duplicate persons not found. Missing IDs: " + missingIds);
        }

        // Check for soft-deleted duplicates
        List<Person> deletedDuplicates = sources.stream()
                .filter(p -> Boolean.TRUE.equals(p.getIsDeleted()))
                .collect(Collectors.toList());
        if (!deletedDuplicates.isEmpty()) {
            List<Long> deletedIds = deletedDuplicates.stream().map(Person::getId).collect(Collectors.toList());
            throw new BadRequestException("One or more duplicate persons have been deleted. Deleted IDs: " + deletedIds);
        }

        // Check for self-merge (shouldn't happen after filtering, but double-check)
        if (uniqueDuplicateIds.contains(targetId)) {
            throw new BadRequestException("Cannot merge person into itself");
        }

        // Merge person data from all sources into target
        for (Person source : sources) {
            mergePersonData(target, source);
        }

        // Note: We don't check for duplicate conflicts here because:
        // 1. The persons being merged are already known duplicates (that's why we're merging them)
        // 2. After merge, the duplicate persons will be soft-deleted, so no conflict will exist
        // 3. If there's a conflict with a different person, it should be handled by the frontend
        //    before calling the merge endpoint (via the duplicate detection on create/update)

        // Update all deals associated with duplicate persons to point to target person
        for (Person source : sources) {
            List<com.brideside.crm.entity.Deal> deals = dealRepository.findByPersonAndIsDeletedFalse(source);
            for (com.brideside.crm.entity.Deal deal : deals) {
                deal.setPerson(target);
            }
            if (!deals.isEmpty()) {
                dealRepository.saveAll(deals);
            }
        }

        // Update all activities associated with duplicate persons to point to target person
        for (Person source : sources) {
            activityRepository.updatePersonId(source.getId(), target.getId());
        }

        // Save the merged target person
        Person saved = repository.save(target);

        // Soft delete duplicate persons
        sources.forEach(p -> p.setIsDeleted(Boolean.TRUE));
        repository.saveAll(sources);

        return PersonMapper.toDto(saved);
    }

    /**
     * Merges data from source person into target person with intelligent conflict resolution.
     * Strategy: If target field is empty/null, use source's value. Otherwise, keep target's value.
     */
    private void mergePersonData(Person target, Person source) {
        // Name: If target is empty, use source's name
        if (isBlank(target.getName()) && StringUtils.hasText(source.getName())) {
            target.setName(source.getName().trim());
        }

        // Phone: Must match (this is why they're being merged), but use non-null value
        if (!StringUtils.hasText(target.getPhone()) && StringUtils.hasText(source.getPhone())) {
            target.setPhone(source.getPhone().trim());
        }

        // Instagram ID: If target is empty, use source's Instagram ID
        if (!StringUtils.hasText(target.getInstagramId()) && StringUtils.hasText(source.getInstagramId())) {
            target.setInstagramId(source.getInstagramId().trim());
        }

        // Email: If target is empty, use source's email
        if (!StringUtils.hasText(target.getEmail()) && StringUtils.hasText(source.getEmail())) {
            target.setEmail(source.getEmail().trim());
        }

        // Lead Date: If target is null, use source's lead date
        if (target.getLeadDate() == null && source.getLeadDate() != null) {
            target.setLeadDate(source.getLeadDate());
        }

        // Venue: If target is empty, use source's venue
        if (!StringUtils.hasText(target.getVenue()) && StringUtils.hasText(source.getVenue())) {
            target.setVenue(source.getVenue().trim());
        }

        // Event Date: If target is empty, use source's event date
        if (!StringUtils.hasText(target.getEventDate()) && StringUtils.hasText(source.getEventDate())) {
            target.setEventDate(source.getEventDate());
        }

        // Organization: If target is null, use source's organization
        if (target.getOrganization() == null && source.getOrganization() != null) {
            target.setOrganization(source.getOrganization());
        }

        // Owner: If target is null, use source's owner
        if (target.getOwner() == null && source.getOwner() != null) {
            target.setOwner(source.getOwner());
        }

        // Category: If target is null, use source's category
        if (target.getCategory() == null && source.getCategory() != null) {
            target.setCategory(source.getCategory());
        }

        // Label: If target is null, use source's label
        if (target.getLabel() == null && source.getLabel() != null) {
            target.setLabel(source.getLabel());
        }

        // Source: If target is null, use source's source
        if (target.getSource() == null && source.getSource() != null) {
            target.setSource(source.getSource());
        }

        // Sub Source: If target is null, use source's sub source
        if (target.getSubSource() == null && source.getSubSource() != null) {
            target.setSubSource(source.getSubSource());
        }
    }

    public List<PersonDTO.OwnerOption> listOwnerOptions() {
        return userRepository.findByActiveTrue().stream()
                .map(this::toOwnerOption)
                .collect(Collectors.toList());
    }

    public List<PersonDTO.EnumOption> listLabelOptions() {
        return PersonDTO.labelOptions();
    }

    public List<CategoryOption> listCategoryOptions() {
        return categoryRepository.findAll(). stream()
                .map(cat -> new CategoryOption(cat.getId(), cat.getName()))
                .collect(Collectors.toList());
    }

    public static class CategoryOption {
        private Long id;
        private String name;

        public CategoryOption(Long id, String name) {
            this.id = id;
            this.name = name;
        }

        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }
    }


    private void apply(PersonDTO dto, Person entity) {
        if (!StringUtils.hasText(dto.getName())) {
            throw new BadRequestException("Person name is required");
        }
        
        // Validate subSource: can only be set when source is DIRECT
        if (dto.getSubSource() != null && dto.getSource() != null && dto.getSource() != com.brideside.crm.entity.DealSource.DIRECT) {
            throw new BadRequestException("Sub source can only be set when source is 'Direct'");
        }
        if (dto.getSubSource() != null && (dto.getSource() == null || dto.getSource() != com.brideside.crm.entity.DealSource.DIRECT)) {
            throw new BadRequestException("Sub source can only be set when source is 'Direct'");
        }
        
        PersonMapper.updateEntity(dto, entity);
        entity.setName(dto.getName().trim());

        if (dto.getOrganizationId() != null) {
            entity.setOrganization(resolveOrganization(dto.getOrganizationId()));
        }

        if (dto.getOwnerId() != null) {
            entity.setOwner(resolveOwner(dto.getOwnerId()));
        }

        if (dto.getCategoryId() != null) {
            entity.setCategory(resolveCategory(dto.getCategoryId()));
        } else {
            entity.setCategory(null);
        }

        if (dto.getLeadDate() == null) {
            if (entity.getLeadDate() == null) {
                entity.setLeadDate(LocalDate.now());
            }
        }
        
        // Clear subSource if source is not DIRECT
        if (dto.getSource() != null && dto.getSource() != com.brideside.crm.entity.DealSource.DIRECT) {
            entity.setSubSource(null);
        }
    }

    private Organization resolveOrganization(Long organizationId) {
        return organizationRepository.findById(organizationId)
                .orElseThrow(() -> new BadRequestException("Organization not found with id " + organizationId));
    }

    private User resolveOwner(Long ownerId) {
        User user = userRepository.findById(ownerId)
                .orElseThrow(() -> new BadRequestException("Owner not found with id " + ownerId));
        if (user.getRole() == null) {
            throw new BadRequestException("Owner must have a role");
        }
        if (Boolean.FALSE.equals(user.getActive())) {
            throw new BadRequestException("Owner must be active");
        }
        return user;
    }

    private Category resolveCategory(Long categoryId) {
        return categoryRepository.findById(categoryId)
                .orElseThrow(() -> new BadRequestException("Category not found with id " + categoryId));
    }

    private PersonDTO.OwnerOption toOwnerOption(User user) {
        PersonDTO.OwnerOption option = new PersonDTO.OwnerOption();
        option.setId(user.getId());
        option.setFirstName(user.getFirstName());
        option.setLastName(user.getLastName());
        option.setEmail(user.getEmail());
        return option;
    }

    private static boolean isBlank(String value) {
        return value == null || value.isBlank();
    }
}

