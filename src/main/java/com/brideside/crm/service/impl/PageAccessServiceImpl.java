package com.brideside.crm.service.impl;

import com.brideside.crm.constants.PageName;
import com.brideside.crm.dto.PageAccessDtos;
import com.brideside.crm.entity.PageAccess;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ForbiddenException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.PageAccessRepository;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.PageAccessService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
public class PageAccessServiceImpl implements PageAccessService {

    @Autowired
    private PageAccessRepository pageAccessRepository;

    @Autowired
    private UserRepository userRepository;

    @Override
    public PageAccessDtos.PageAccessResponse getUserPageAccess(Long userId, String currentUserEmail) {
        User currentUser = getCurrentUser(currentUserEmail);
        User targetUser = userRepository.findById(userId)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with id: " + userId));

        // Check permissions: Admin can view anyone's access, users can view their own
        if (!currentUser.getRole().getName().equals(Role.RoleName.ADMIN) 
                && !currentUser.getId().equals(userId)) {
            throw new ForbiddenException("You don't have permission to view this user's page access");
        }

        Map<String, Boolean> pageAccess = getUserPageAccessMap(targetUser);
        
        PageAccessDtos.PageAccessResponse response = new PageAccessDtos.PageAccessResponse();
        response.setUserId(userId);
        response.setPageAccess(pageAccess);
        return response;
    }

    @Override
    public PageAccessDtos.PageAccessResponse getCurrentUserPageAccess(String currentUserEmail) {
        User user = getCurrentUser(currentUserEmail);
        Map<String, Boolean> pageAccess = getUserPageAccessMap(user);
        
        PageAccessDtos.PageAccessResponse response = new PageAccessDtos.PageAccessResponse();
        response.setUserId(user.getId());
        response.setPageAccess(pageAccess);
        return response;
    }

    @Override
    @Transactional
    public PageAccessDtos.PageAccessResponse updateUserPageAccess(
            Long userId, 
            PageAccessDtos.UpdatePageAccessRequest request, 
            String currentUserEmail) {
        
        User currentUser = getCurrentUser(currentUserEmail);
        
        // Only admin can update page access
        if (!currentUser.getRole().getName().equals(Role.RoleName.ADMIN)) {
            throw new ForbiddenException("Only administrators can update page access");
        }

        User targetUser = userRepository.findById(userId)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with id: " + userId));

        // Validate all page names
        for (String pageName : request.getPageAccess().keySet()) {
            if (!PageName.isValid(pageName)) {
                throw new BadRequestException("Invalid page name: " + pageName);
            }
        }

        // Update or create page access records
        for (Map.Entry<String, Boolean> entry : request.getPageAccess().entrySet()) {
            String pageName = entry.getKey();
            Boolean hasAccess = entry.getValue();

            PageAccess pageAccess = pageAccessRepository
                    .findByUserIdAndPageName(userId, pageName)
                    .orElse(new PageAccess());

            if (pageAccess.getId() == null) {
                pageAccess.setUser(targetUser);
                pageAccess.setPageName(pageName);
            }
            pageAccess.setHasAccess(hasAccess);
            pageAccessRepository.save(pageAccess);
        }

        // Return updated page access
        Map<String, Boolean> updatedPageAccess = getUserPageAccessMap(targetUser);
        
        PageAccessDtos.PageAccessResponse response = new PageAccessDtos.PageAccessResponse();
        response.setUserId(userId);
        response.setPageAccess(updatedPageAccess);
        return response;
    }

    @Override
    @Transactional
    public PageAccessDtos.SinglePageAccessResponse updateSinglePageAccess(
            Long userId, 
            String pageName, 
            PageAccessDtos.UpdateSinglePageAccessRequest request, 
            String currentUserEmail) {
        
        User currentUser = getCurrentUser(currentUserEmail);
        
        // Only admin can update page access
        if (!currentUser.getRole().getName().equals(Role.RoleName.ADMIN)) {
            throw new ForbiddenException("Only administrators can update page access");
        }

        // Validate page name
        if (!PageName.isValid(pageName)) {
            throw new BadRequestException("Invalid page name: " + pageName);
        }

        User targetUser = userRepository.findById(userId)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with id: " + userId));

        PageAccess pageAccess = pageAccessRepository
                .findByUserIdAndPageName(userId, pageName)
                .orElse(new PageAccess());

        if (pageAccess.getId() == null) {
            pageAccess.setUser(targetUser);
            pageAccess.setPageName(pageName);
        }
        pageAccess.setHasAccess(request.getHasAccess());
        pageAccessRepository.save(pageAccess);

        PageAccessDtos.SinglePageAccessResponse response = new PageAccessDtos.SinglePageAccessResponse();
        response.setUserId(userId);
        response.setPageName(pageName);
        response.setHasAccess(request.getHasAccess());
        return response;
    }

    @Override
    public List<PageAccessDtos.PageAccessSummaryResponse> getAllUsersPageAccessSummary(String currentUserEmail) {
        User currentUser = getCurrentUser(currentUserEmail);
        
        // Only admin can view all users' page access summary
        if (!currentUser.getRole().getName().equals(Role.RoleName.ADMIN)) {
            throw new ForbiddenException("Only administrators can view page access summary");
        }

        List<User> allUsers = userRepository.findAll();
        
        return allUsers.stream()
                .map(user -> {
                    Map<String, Boolean> pageAccess = getUserPageAccessMap(user);
                    
                    PageAccessDtos.PageAccessSummaryResponse response = new PageAccessDtos.PageAccessSummaryResponse();
                    response.setUserId(user.getId());
                    response.setEmail(user.getEmail());
                    response.setFirstName(user.getFirstName());
                    response.setLastName(user.getLastName());
                    response.setRole(user.getRole().getName().name());
                    response.setPageAccess(pageAccess);
                    return response;
                })
                .collect(Collectors.toList());
    }

    /**
     * Get page access map for a user, applying role-based defaults if no records exist
     */
    private Map<String, Boolean> getUserPageAccessMap(User user) {
        List<PageAccess> pageAccessList = pageAccessRepository.findByUser(user);
        Map<String, Boolean> pageAccessMap = new HashMap<>();

        // Get all page names
        List<String> allPageNames = PageName.getAllPageNames();

        // If user has page access records, use them
        if (!pageAccessList.isEmpty()) {
            // Initialize all pages to false
            for (String pageName : allPageNames) {
                pageAccessMap.put(pageName, false);
            }
            // Set access from records
            for (PageAccess pageAccess : pageAccessList) {
                pageAccessMap.put(pageAccess.getPageName(), pageAccess.getHasAccess());
            }
        } else {
            // No records exist, apply role-based defaults
            pageAccessMap = getRoleBasedDefaultAccess(user.getRole().getName());
        }

        // Admin always has access to page access management
        if (user.getRole().getName().equals(Role.RoleName.ADMIN)) {
            pageAccessMap.put(PageName.PAGE_ACCESS_MANAGEMENT, true);
        }

        return pageAccessMap;
    }

    /**
     * Get role-based default page access
     * Option B: Role-Based Default as recommended in the spec
     */
    private Map<String, Boolean> getRoleBasedDefaultAccess(Role.RoleName roleName) {
        Map<String, Boolean> defaults = new HashMap<>();
        List<String> allPageNames = PageName.getAllPageNames();

        // Initialize all to false
        for (String pageName : allPageNames) {
            defaults.put(pageName, false);
        }

        switch (roleName) {
            case ADMIN:
                // Admin: All pages enabled by default
                for (String pageName : allPageNames) {
                    defaults.put(pageName, true);
                }
                break;
            case CATEGORY_MANAGER:
                // Category Manager: Default pages for category manager role
                defaults.put(PageName.PERSONS, true);
                defaults.put(PageName.DEALS, true);
                defaults.put(PageName.CALENDAR, true);
                defaults.put(PageName.TEAMS, true);
                defaults.put(PageName.ACTIVITIES, true);
                defaults.put(PageName.ORGANIZATIONS, true);
                defaults.put(PageName.TARGETS, true);
                defaults.put(PageName.PIPELINES, true);
                defaults.put(PageName.DASHBOARD_CATEGORY_MANAGER, true);
                defaults.put(PageName.REPORTS_DEAL_SOURCE, true);
                defaults.put(PageName.REPORTS_DEAL_SUB_SOURCE, true);
                defaults.put(PageName.REPORTS_DEAL_STATUS, true);
                defaults.put(PageName.REPORTS_DEAL_LOST_REASON, true);
                defaults.put(PageName.REPORTS_DEAL_DURATION, true);
                break;
            case SALES:
                // Sales: Default pages for sales role
                defaults.put(PageName.PERSONS, true);
                defaults.put(PageName.DEALS, true);
                defaults.put(PageName.CALENDAR, true);
                defaults.put(PageName.ACTIVITIES, true);
                defaults.put(PageName.ORGANIZATIONS, true);
                defaults.put(PageName.TARGETS, true);
                defaults.put(PageName.PIPELINES, true);
                defaults.put(PageName.DASHBOARD_SALES, true);
                defaults.put(PageName.REPORTS_DEAL_SOURCE, true);
                defaults.put(PageName.REPORTS_DEAL_SUB_SOURCE, true);
                defaults.put(PageName.REPORTS_DEAL_STATUS, true);
                defaults.put(PageName.REPORTS_DEAL_LOST_REASON, true);
                defaults.put(PageName.REPORTS_DEAL_DURATION, true);
                break;
            case PRESALES:
                // Presales: Default pages for presales role
                defaults.put(PageName.PERSONS, true);
                defaults.put(PageName.DEALS, true);
                defaults.put(PageName.CALENDAR, true);
                defaults.put(PageName.ACTIVITIES, true);
                defaults.put(PageName.ORGANIZATIONS, true);
                defaults.put(PageName.DASHBOARD_PRE_SALES, true);
                break;
            default:
                // Unknown role: no access by default
                break;
        }

        return defaults;
    }

    /**
     * Get current user by email
     */
    private User getCurrentUser(String email) {
        return userRepository.findByEmail(email)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with email: " + email));
    }
}

