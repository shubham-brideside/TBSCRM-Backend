package com.brideside.crm.service;

import com.brideside.crm.dto.PersonDTO;
import com.brideside.crm.dto.PersonSummaryDTO;
import com.brideside.crm.entity.Category;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Person;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.mapper.PersonMapper;
import com.brideside.crm.repository.CategoryRepository;
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

    public PersonService(PersonRepository repository,
                         OrganizationRepository organizationRepository,
                         UserRepository userRepository,
                         CategoryRepository categoryRepository,
                         LabelRepository labelRepository) {
        this.repository = repository;
        this.organizationRepository = organizationRepository;
        this.userRepository = userRepository;
        this.categoryRepository = categoryRepository;
        this.labelRepository = labelRepository;
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
        Person entity = repository.findById(id)
                .orElseThrow(() -> new NoSuchElementException("Person not found"));
        
        // Check if person is soft-deleted
        if (Boolean.TRUE.equals(entity.getIsDeleted())) {
            throw new NoSuchElementException("Person not found");
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
        if (duplicateIds == null || duplicateIds.isEmpty()) {
            return get(targetId);
        }

        Person target = repository.findById(targetId)
                .orElseThrow(() -> new NoSuchElementException("Person not found"));
        
        // Check if target person is soft-deleted
        if (Boolean.TRUE.equals(target.getIsDeleted())) {
            throw new NoSuchElementException("Person not found");
        }
        
        List<Person> sources = repository.findAllById(duplicateIds);

        for (Person source : sources) {
            if (isBlank(target.getName()) && StringUtils.hasText(source.getName())) {
                target.setName(source.getName());
            }
            if (!StringUtils.hasText(target.getInstagramId()) && StringUtils.hasText(source.getInstagramId())) {
                target.setInstagramId(source.getInstagramId());
            }
            if (!StringUtils.hasText(target.getPhone()) && StringUtils.hasText(source.getPhone())) {
                target.setPhone(source.getPhone());
            }
            if (!StringUtils.hasText(target.getEmail()) && StringUtils.hasText(source.getEmail())) {
                target.setEmail(source.getEmail());
            }
            if (target.getLeadDate() == null && source.getLeadDate() != null) {
                target.setLeadDate(source.getLeadDate());
            }
            if (!StringUtils.hasText(target.getVenue()) && StringUtils.hasText(source.getVenue())) {
                target.setVenue(source.getVenue());
            }
            if (target.getEventDate() == null && source.getEventDate() != null) {
                target.setEventDate(source.getEventDate());
            }
            if (target.getOrganization() == null && source.getOrganization() != null) {
                target.setOrganization(source.getOrganization());
            }
            if (target.getOwner() == null && source.getOwner() != null) {
                target.setOwner(source.getOwner());
            }
            if (target.getLabel() == null && source.getLabel() != null) {
                target.setLabel(source.getLabel());
            }
            if (target.getSource() == null && source.getSource() != null) {
                target.setSource(source.getSource());
            }
        }

        Person saved = repository.save(target);
        List<Long> toDelete = duplicateIds.stream()
                .filter(id -> !id.equals(targetId))
                .collect(Collectors.toList());
        if (!toDelete.isEmpty()) {
            // Soft delete duplicate persons instead of hard delete
            List<Person> duplicates = repository.findAllById(toDelete);
            duplicates.forEach(p -> p.setIsDeleted(Boolean.TRUE));
            repository.saveAll(duplicates);
        }
        return PersonMapper.toDto(saved);
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
        return categoryRepository.findAll().stream()
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

