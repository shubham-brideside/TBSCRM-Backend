package com.brideside.crm.service;

import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.Team;
import com.brideside.crm.entity.User;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.repository.TeamRepository;
import com.brideside.crm.repository.UserRepository;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Computes allowed organization and user scopes for the currently logged-in user.
 * This is used by the Activities endpoints to enforce role-based visibility.
 */
@Service
public class ActivityScopeService {

    public record Scope(Set<Long> organizationIds,
                        Set<String> organizationNames,
                        Set<Long> assignedUserIds,
                        Set<String> assignedUserEmails) {}
    public record FilterContext(boolean restricted,
                                List<Organization> organizations,
                                List<User> users) {}

    private final UserRepository userRepository;
    private final OrganizationRepository organizationRepository;
    private final TeamRepository teamRepository;

    public ActivityScopeService(UserRepository userRepository,
                                OrganizationRepository organizationRepository,
                                TeamRepository teamRepository) {
        this.userRepository = userRepository;
        this.organizationRepository = organizationRepository;
        this.teamRepository = teamRepository;
    }

    public Optional<User> currentUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !(authentication.getPrincipal() instanceof UserDetails)) {
            return Optional.empty();
        }
        String email = ((UserDetails) authentication.getPrincipal()).getUsername();
        return userRepository.findByEmail(email);
    }

    /**
     * Get the current user's role name, if available.
     * Returns null if user is not authenticated or has no role.
     */
    public Role.RoleName getCurrentUserRole() {
        Optional<User> userOpt = currentUser();
        if (userOpt.isEmpty() || userOpt.get().getRole() == null) {
            return null;
        }
        return userOpt.get().getRole().getName();
    }

    public Scope resolveScope() {
        Optional<User> userOpt = currentUser();
        if (userOpt.isEmpty() || userOpt.get().getRole() == null) {
            return new Scope(Set.of(), Set.of(), Set.of(), Set.of());
        }
        User currentUser = userOpt.get();
        Role.RoleName roleName = currentUser.getRole().getName();

        Set<Long> orgIds = new HashSet<>();
        Set<String> orgNames = new HashSet<>();
        Set<Long> userIds = new HashSet<>();
        Set<String> userEmails = new HashSet<>();

        if (roleName == Role.RoleName.ADMIN) {
            // Admin: unrestricted – empty sets mean "no restriction" to callers that know how to interpret it
            return new Scope(Set.of(), Set.of(), Set.of(), Set.of());
        } else if (roleName == Role.RoleName.CATEGORY_MANAGER) {
            // Category Manager sees activities for:
            // 1. Organizations owned by their Sales reports (Sales users they manage)
            //    (These appear in "All Organizations" filter dropdown for filtering purposes)
            //    NOTE: Activities are NOT shown based on organization ownership - only by assignment
            // 2. Activities assigned to: Category Manager + their Sales reports + Presales under those Sales
            //    Activities assigned to other Sales users or Presales under other Sales managers are NOT visible
            
            // Start with the Category Manager themselves
            userIds.add(currentUser.getId());
            addEmail(userEmails, currentUser.getEmail());
            
            // Find all direct reports (Sales users managed by this Category Manager)
            Set<Long> salesIds = new HashSet<>();
            List<User> allUsers = userRepository.findAll();
            
            for (User user : allUsers) {
                if (user.getId() == null || user.getRole() == null) continue;
                
                // Skip self
                if (user.getId().equals(currentUser.getId())) {
                    continue;
                }

                // Direct reports (Sales users directly under Category Manager)
                if (user.getManager() != null && currentUser.getId().equals(user.getManager().getId())) {
                    userIds.add(user.getId());
                    addEmail(userEmails, user.getEmail());
                    if (user.getRole().getName() == Role.RoleName.SALES) {
                        salesIds.add(user.getId());
                    }
                }

                // Second-level (Presales -> Sales -> Category Manager)
                if (user.getManager() != null &&
                        user.getManager().getManager() != null &&
                        currentUser.getId().equals(user.getManager().getManager().getId())) {
                    userIds.add(user.getId());
                    addEmail(userEmails, user.getEmail());
                }
            }

            // Get organizations owned by Sales users under this Category Manager
            // Also include organizations directly owned by the Category Manager (if any)
            // NOTE: These are included for filter dropdown purposes, but NOT used for activity visibility
            if (!salesIds.isEmpty() || currentUser.getId() != null) {
                List<Organization> organizations = organizationRepository.findAll().stream()
                        .filter(org -> org.getOwner() != null && org.getOwner().getId() != null)
                        .filter(org -> {
                            Long ownerId = org.getOwner().getId();
                            // Include orgs owned by Sales reports OR by the Category Manager themselves
                            return salesIds.contains(ownerId) || 
                                   (currentUser.getId() != null && currentUser.getId().equals(ownerId));
                        })
                        .collect(Collectors.toList());
                organizations.forEach(org -> {
                    if (org.getId() != null) {
                        orgIds.add(org.getId());
                    }
                    addName(orgNames, org.getName());
                });
            }
        } else if (roleName == Role.RoleName.SALES) {
            // Sales user sees:
            // 1. Organizations: All organizations owned by this Sales user
            //    (These appear in "All Organizations" filter dropdown for filtering purposes)
            //    NOTE: Activities are NOT shown based on organization ownership - only by assignment
            List<Organization> organizations = organizationRepository.findByOwner_Id(currentUser.getId());
            organizations.forEach(org -> {
                if (org.getId() != null) {
                    orgIds.add(org.getId());
                }
                addName(orgNames, org.getName());
            });

            // 2. Users: This Sales user + all Presales team members under them
            //    Activities assigned to these users will be visible
            //    Activities assigned to other Sales users or Presales under other Sales managers are NOT visible
            userIds.add(currentUser.getId());
            addEmail(userEmails, currentUser.getEmail());
            List<Team> teams = teamRepository.findByManager_Id(currentUser.getId());
            for (Team team : teams) {
                for (User member : team.getMembers()) {
                    if (member.getId() != null) {
                        userIds.add(member.getId());
                        addEmail(userEmails, member.getEmail());
                    }
                }
            }
        } else if (roleName == Role.RoleName.PRESALES) {
            // Presales user sees:
            // 1. Organizations: All organizations owned by their Sales manager
            //    (These appear in "All Organizations" filter dropdown for filtering purposes)
            //    NOTE: Activities are NOT shown based on organization ownership - only by assignment
            // 2. Users: Themselves + their Sales manager
            //    Activities assigned to these users will be visible
            //    Activities assigned to other users are NOT visible
            
            // Add themselves for activity assignment visibility
            userIds.add(currentUser.getId());
            addEmail(userEmails, currentUser.getEmail());

            // Find their Sales manager via teams and add to assigned user IDs
            List<Team> teams = teamRepository.findByMembers_Id(currentUser.getId());
            Set<Long> managerIds = teams.stream()
                    .map(Team::getManager)
                    .filter(mgr -> mgr != null && mgr.getId() != null)
                    .map(User::getId)
                    .collect(Collectors.toSet());

            // Add Sales manager(s) to the assigned user IDs so Presales can see their activities
            for (Long managerId : managerIds) {
                userIds.add(managerId);
                // Also add the manager's email if available
                userRepository.findById(managerId).ifPresent(manager -> {
                    if (manager.getEmail() != null) {
                        addEmail(userEmails, manager.getEmail());
                    }
                });
            }

            // Get all organizations owned by their Sales manager(s)
            // NOTE: These are included for filter dropdown purposes, but NOT used for activity visibility
            if (!managerIds.isEmpty()) {
                List<Organization> organizations = organizationRepository.findAll().stream()
                        .filter(org -> org.getOwner() != null && org.getOwner().getId() != null)
                        .filter(org -> managerIds.contains(org.getOwner().getId()))
                        .collect(Collectors.toList());
                organizations.forEach(org -> {
                    if (org.getId() != null) {
                        orgIds.add(org.getId());
                    }
                    addName(orgNames, org.getName());
                });
            }
        }

        return new Scope(orgIds, orgNames, userIds, userEmails);
    }

    /**
     * Resolve data for the /api/activities/filters endpoint.
     * Admins get full org/user lists with restricted=false.
     * Other roles get only the scoped organizations/users and restricted=true.
     */
    public FilterContext resolveFilterContext() {
        Optional<User> userOpt = currentUser();
        if (userOpt.isEmpty() || userOpt.get().getRole() == null) {
            // Fallback: treat as unrestricted
            return new FilterContext(
                    false,
                    organizationRepository.findAll(),
                    userRepository.findAll()
            );
        }

        User currentUser = userOpt.get();
        Role.RoleName roleName = currentUser.getRole().getName();

        if (roleName == Role.RoleName.ADMIN) {
            return new FilterContext(
                    false,
                    organizationRepository.findAll(),
                    userRepository.findAll()
            );
        }

        Scope scope = resolveScope();
        List<Organization> orgs = scope.organizationIds().isEmpty()
                ? List.of()
                : organizationRepository.findAllById(scope.organizationIds());
        
        // For Presales: return empty users list since they don't have users under them
        // "All Users" filter should be hidden/disabled for Presales
        List<User> users;
        if (roleName == Role.RoleName.PRESALES) {
            users = List.of(); // Empty list - Presales don't have users under them
        } else {
            users = scope.assignedUserIds().isEmpty()
                    ? List.of(currentUser)
                    : userRepository.findAllById(scope.assignedUserIds());
        }

        return new FilterContext(true, orgs, users);
    }

    private void addName(Set<String> target, String value) {
        String normalized = normalize(value);
        if (normalized != null) {
            target.add(normalized);
        }
    }

    private void addEmail(Set<String> target, String value) {
        String normalized = normalize(value);
        if (normalized != null) {
            target.add(normalized);
        }
    }

    private String normalize(String value) {
        if (value == null) {
            return null;
        }
        String trimmed = value.trim();
        if (trimmed.isEmpty()) {
            return null;
        }
        return trimmed.toLowerCase();
    }
}


