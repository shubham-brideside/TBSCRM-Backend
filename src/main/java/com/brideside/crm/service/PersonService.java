package com.brideside.crm.service;

import com.brideside.crm.dto.PersonDTO;
import com.brideside.crm.dto.PersonSummaryDTO;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Person;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.mapper.PersonMapper;
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
import java.util.EnumSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Transactional
public class PersonService {

    private static final Set<Role.RoleName> ALLOWED_OWNER_ROLES = EnumSet.of(Role.RoleName.SALES);

    private final PersonRepository repository;
    private final OrganizationRepository organizationRepository;
    private final UserRepository userRepository;

    public PersonService(PersonRepository repository,
                         OrganizationRepository organizationRepository,
                         UserRepository userRepository) {
        this.repository = repository;
        this.organizationRepository = organizationRepository;
        this.userRepository = userRepository;
    }

    public Page<PersonDTO> list(String q,
                                Person.PersonLabel label,
                                Long organizationId,
                                Long ownerId,
                                com.brideside.crm.entity.DealSource source,
                                LocalDate leadDateFrom,
                                LocalDate leadDateTo,
                                Pageable pageable) {

        Specification<Person> spec = Specification.where(PersonSpecifications.search(q))
                .and(PersonSpecifications.hasLabel(label))
                .and(PersonSpecifications.hasOrganization(organizationId))
                .and(PersonSpecifications.hasOwner(ownerId))
                .and(PersonSpecifications.hasSource(source))
                .and(PersonSpecifications.leadDateBetween(leadDateFrom, leadDateTo));

        return repository.findAll(spec, pageable).map(PersonMapper::toDto);
    }

    public PersonDTO get(Long id) {
        return repository.findById(id)
                .map(PersonMapper::toDto)
                .orElseThrow(() -> new NoSuchElementException("Person not found"));
    }

    public PersonSummaryDTO getSummary(Long id) {
        Person person = repository.findById(id)
                .orElseThrow(() -> new NoSuchElementException("Person not found"));
        PersonSummaryDTO summary = new PersonSummaryDTO();
        summary.setPerson(PersonMapper.toDto(person));
        summary.setDealsCount(0L);
        return summary;
    }

    public PersonDTO create(PersonDTO dto) {
        Person entity = new Person();
        apply(dto, entity);
        Person saved = repository.save(entity);
        return PersonMapper.toDto(saved);
    }

    public PersonDTO update(Long id, PersonDTO dto) {
        Person entity = repository.findById(id)
                .orElseThrow(() -> new NoSuchElementException("Person not found"));
        apply(dto, entity);
        Person saved = repository.save(entity);
        return PersonMapper.toDto(saved);
    }

    public void delete(Long id) {
        repository.deleteById(id);
    }

    public long bulkDelete(List<Long> ids) {
        List<Person> all = repository.findAllById(ids);
        repository.deleteAllInBatch(all);
        return all.size();
    }

    public PersonDTO merge(Long targetId, List<Long> duplicateIds) {
        if (duplicateIds == null || duplicateIds.isEmpty()) {
            return get(targetId);
        }

        Person target = repository.findById(targetId)
                .orElseThrow(() -> new NoSuchElementException("Person not found"));
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
            repository.deleteAllByIdInBatch(toDelete);
        }
        return PersonMapper.toDto(saved);
    }

    public List<PersonDTO.OwnerOption> listOwnerOptions() {
        return userRepository.findByRole_NameInAndActiveTrue(ALLOWED_OWNER_ROLES).stream()
                .map(this::toOwnerOption)
                .collect(Collectors.toList());
    }

    public List<PersonDTO.EnumOption> listLabelOptions() {
        return PersonDTO.labelOptions();
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
        if (user.getRole() == null || !ALLOWED_OWNER_ROLES.contains(user.getRole().getName())) {
            throw new BadRequestException("Owner must have SALES role");
        }
        if (Boolean.FALSE.equals(user.getActive())) {
            throw new BadRequestException("Owner must be active");
        }
        return user;
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

