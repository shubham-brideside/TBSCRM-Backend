package com.brideside.crm.service;

import com.brideside.crm.dto.PersonDTO;
import com.brideside.crm.dto.PersonSummaryDTO;
import com.brideside.crm.entity.Person;
import com.brideside.crm.mapper.PersonMapper;
import com.brideside.crm.repository.PersonRepository;
import com.brideside.crm.person.MergeRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

import static com.brideside.crm.repository.PersonSpecifications.*;

@Service
@Transactional
public class PersonService {
    private final PersonRepository repository;

    public PersonService(PersonRepository repository) {
        this.repository = repository;
    }

    public Page<PersonDTO> list(String q, String category, String organization, String manager, String dateFrom, String dateTo, String weddingVenue, String weddingDate, Pageable pageable) {
        Specification<Person> spec = Specification.where(null);

        Specification<Person> searchSpec = search(q);
        if (searchSpec != null) spec = spec.and(searchSpec);

        Specification<Person> categorySpec = equalsField("category", category);
        if (categorySpec != null) spec = spec.and(categorySpec);

        Specification<Person> orgSpec = equalsField("organization", organization);
        if (orgSpec != null) spec = spec.and(orgSpec);

        Specification<Person> managerSpec = equalsField("manager", manager);
        if (managerSpec != null) spec = spec.and(managerSpec);

        java.time.format.DateTimeFormatter fmt = java.time.format.DateTimeFormatter.ofPattern("dd/MM/yyyy");
        java.time.Instant fromInstant = null;
        java.time.Instant toInstantExclusive = null;
        if (dateFrom != null && !dateFrom.isBlank()) {
            java.time.LocalDate d = java.time.LocalDate.parse(dateFrom, fmt);
            fromInstant = d.atStartOfDay(java.time.ZoneId.systemDefault()).toInstant();
        }
        if (dateTo != null && !dateTo.isBlank()) {
            java.time.LocalDate d = java.time.LocalDate.parse(dateTo, fmt).plusDays(1);
            toInstantExclusive = d.atStartOfDay(java.time.ZoneId.systemDefault()).toInstant();
        }

        Specification<Person> dateSpec = createdAtBetween(fromInstant, toInstantExclusive);
        if (dateSpec != null) spec = spec.and(dateSpec);

        Specification<Person> venueSpec = equalsField("venue", weddingVenue);
        if (venueSpec != null) spec = spec.and(venueSpec);

        Specification<Person> wedDateEq = equalsField("weddingDate", weddingDate);
        if (wedDateEq != null) spec = spec.and(wedDateEq);

        return repository.findAll(spec, pageable).map(PersonMapper::toDto);
    }

    public Map<String, List<String>> getFilterMeta() {
        Map<String, List<String>> meta = new HashMap<>();
        meta.put("categories", repository.findDistinctCategories());
        meta.put("organizations", repository.findDistinctOrganizations());
        meta.put("managers", repository.findDistinctManagers());
        meta.put("venues", repository.findDistinctVenues());
        return meta;
    }

    public List<String> getManagersByCategory(String category) {
        if (category == null || category.isBlank()) return repository.findDistinctManagers();
        return repository.findManagersByCategory(category);
    }

    public List<String> getManagersByOrganization(String organization) {
        if (organization == null || organization.isBlank()) return repository.findDistinctManagers();
        return repository.findManagersByOrganization(organization);
    }

    public PersonDTO get(Long id) {
        return repository.findById(id).map(PersonMapper::toDto).orElseThrow(() -> new NoSuchElementException("Person not found"));
    }

    public PersonSummaryDTO getSummary(Long id) {
        Person person = repository.findById(id).orElseThrow(() -> new NoSuchElementException("Person not found"));
        PersonSummaryDTO summary = new PersonSummaryDTO();
        summary.setPerson(PersonMapper.toDto(person));
        summary.setDealsCount(0L);
        return summary;
    }

    public PersonDTO create(PersonDTO dto) {
        Person e = new Person();
        if (dto.getCreatedDate() == null) dto.setCreatedDate(java.time.LocalDate.now().format(java.time.format.DateTimeFormatter.ofPattern("dd/MM/yyyy")));
        PersonMapper.updateEntity(dto, e);
        return PersonMapper.toDto(repository.save(e));
    }

    public PersonDTO update(Long id, PersonDTO dto) {
        Person e = repository.findById(id).orElseThrow(() -> new NoSuchElementException("Person not found"));
        PersonMapper.updateEntity(dto, e);
        return PersonMapper.toDto(repository.save(e));
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
        if (duplicateIds == null || duplicateIds.isEmpty()) return get(targetId);
        Person target = repository.findById(targetId).orElseThrow(() -> new NoSuchElementException("Person not found"));
        List<Person> sources = repository.findAllById(duplicateIds);

        for (Person s : sources) {
            if (isBlank(target.getName()) && !isBlank(s.getName())) target.setName(s.getName());
            if (isBlank(target.getInstagramId()) && !isBlank(s.getInstagramId())) target.setInstagramId(s.getInstagramId());
            if (isBlank(target.getPhone()) && !isBlank(s.getPhone())) target.setPhone(s.getPhone());
            if (isBlank(target.getWeddingDate()) && !isBlank(s.getWeddingDate())) target.setWeddingDate(s.getWeddingDate());
            if (isBlank(target.getVenue()) && !isBlank(s.getVenue())) target.setVenue(s.getVenue());
            if (isBlank(target.getOrganization()) && !isBlank(s.getOrganization())) target.setOrganization(s.getOrganization());
            if (isBlank(target.getManager()) && !isBlank(s.getManager())) target.setManager(s.getManager());
            if (isBlank(target.getCategory()) && !isBlank(s.getCategory())) target.setCategory(s.getCategory());
            if (isBlank(target.getSource()) && !isBlank(s.getSource())) target.setSource(s.getSource());
            if (isBlank(target.getCreatedDate()) && !isBlank(s.getCreatedDate())) target.setCreatedDate(s.getCreatedDate());
            if (isBlank(target.getEventType()) && !isBlank(s.getEventType())) target.setEventType(s.getEventType());
        }

        Person saved = repository.save(target);
        List<Long> toDelete = new java.util.ArrayList<>();
        for (Long id : duplicateIds) {
            if (!id.equals(targetId)) toDelete.add(id);
        }
        if (!toDelete.isEmpty()) repository.deleteAllByIdInBatch(toDelete);
        return PersonMapper.toDto(saved);
    }

    private static boolean isBlank(String s) { return s == null || s.isBlank(); }
}


