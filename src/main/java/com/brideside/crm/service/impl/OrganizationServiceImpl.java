package com.brideside.crm.service.impl;

import com.brideside.crm.dto.OrganizationDtos;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.OrganizationService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Transactional
public class OrganizationServiceImpl implements OrganizationService {

    private final OrganizationRepository organizationRepository;
    private final UserRepository userRepository;
    private static final Set<Role.RoleName> ALLOWED_OWNER_ROLES =
            EnumSet.of(Role.RoleName.SALES, Role.RoleName.CATEGORY_MANAGER);

    public OrganizationServiceImpl(OrganizationRepository organizationRepository,
                                   UserRepository userRepository) {
        this.organizationRepository = organizationRepository;
        this.userRepository = userRepository;
    }

    @Override
    public OrganizationDtos.OrganizationResponse create(OrganizationDtos.OrganizationRequest request) {
        Organization organization = new Organization();
        organization.setName(trimmed(request.getName()));
        organization.setOwner(resolveOwner(request.getOwnerId()));
        organization.setCategory(resolveCategory(request.getCategory()));
        organization.setAddress(trimmed(request.getAddress()));
        return toResponse(organizationRepository.save(organization));
    }

    @Override
    @Transactional(readOnly = true)
    public List<OrganizationDtos.OrganizationResponse> list() {
        return organizationRepository.findAll()
                .stream()
                .map(this::toResponse)
                .collect(Collectors.toList());
    }

    @Override
    @Transactional(readOnly = true)
    public OrganizationDtos.OrganizationResponse get(Long id) {
        return toResponse(getOrThrow(id));
    }

    @Override
    public OrganizationDtos.OrganizationResponse update(Long id, OrganizationDtos.OrganizationRequest request) {
        Organization organization = getOrThrow(id);
        organization.setName(trimmed(request.getName()));
        organization.setOwner(resolveOwner(request.getOwnerId()));
        organization.setCategory(resolveCategory(request.getCategory()));
        organization.setAddress(trimmed(request.getAddress()));
        organization.setGoogleCalendarId(trimmed(request.getGoogleCalendarId()));
        return toResponse(organizationRepository.save(organization));
    }

    @Override
    public void delete(Long id) {
        Organization organization = getOrThrow(id);
        organizationRepository.delete(organization);
    }

    @Override
    @Transactional(readOnly = true)
    public List<OrganizationDtos.OwnerOption> listOwnerOptions() {
        return userRepository.findByRole_NameInAndActiveTrue(ALLOWED_OWNER_ROLES)
                .stream()
                .map(this::toOwnerOption)
                .collect(Collectors.toList());
    }

    @Override
    @Transactional(readOnly = true)
    public List<OrganizationDtos.CategoryOption> listCategoryOptions() {
        return OrganizationDtos.allCategoryOptions();
    }

    private Organization getOrThrow(Long id) {
        return organizationRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Organization not found with id " + id));
    }

    private OrganizationDtos.OrganizationResponse toResponse(Organization organization) {
        OrganizationDtos.OrganizationResponse response = new OrganizationDtos.OrganizationResponse();
        response.setId(organization.getId());
        response.setName(organization.getName());
        response.setOwner(toOwnerSummary(organization.getOwner()));
        response.setCategory(organization.getCategory());
        response.setAddress(organization.getAddress());
        response.setGoogleCalendarId(organization.getGoogleCalendarId());
        response.setCreatedAt(organization.getCreatedAt());
        response.setUpdatedAt(organization.getUpdatedAt());
        return response;
    }

    private OrganizationDtos.OwnerSummary toOwnerSummary(User user) {
        if (user == null) {
            return null;
        }
        OrganizationDtos.OwnerSummary summary = new OrganizationDtos.OwnerSummary();
        summary.setId(user.getId());
        summary.setFirstName(user.getFirstName());
        summary.setLastName(user.getLastName());
        summary.setEmail(user.getEmail());
        summary.setRole(user.getRole() != null ? user.getRole().getName().name() : null);
        return summary;
    }

    private OrganizationDtos.OwnerOption toOwnerOption(User user) {
        OrganizationDtos.OwnerOption option = new OrganizationDtos.OwnerOption();
        option.setId(user.getId());
        option.setFirstName(user.getFirstName());
        option.setLastName(user.getLastName());
        option.setEmail(user.getEmail());
        option.setRole(user.getRole() != null ? user.getRole().getName().name() : null);
        return option;
    }

    private User resolveOwner(Long ownerId) {
        if (ownerId == null) {
            return null;
        }
        User user = userRepository.findById(ownerId)
                .orElseThrow(() -> new BadRequestException("Owner user not found with id " + ownerId));
        if (user.getRole() == null || !ALLOWED_OWNER_ROLES.contains(user.getRole().getName())) {
            throw new BadRequestException("Owner must have role SALES or CATEGORY_MANAGER");
        }
        if (Boolean.FALSE.equals(user.getActive())) {
            throw new BadRequestException("Owner must be an active user");
        }
        return user;
    }

    private Organization.OrganizationCategory resolveCategory(Organization.OrganizationCategory category) {
        if (category == null) {
            throw new BadRequestException("Organization category is required");
        }
        return category;
    }

    private String trimmed(String value) {
        return value == null ? null : value.trim();
    }
}


