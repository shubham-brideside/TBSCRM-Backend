package com.brideside.crm.service.impl;

import com.brideside.crm.dto.CreateUserRequest;
import com.brideside.crm.dto.SetPasswordRequest;
import com.brideside.crm.dto.TeamDtos;
import com.brideside.crm.dto.UpdateUserRequest;
import com.brideside.crm.dto.UserResponse;
import com.brideside.crm.entity.InvitationToken;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Person;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.Team;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.constants.PageName;
import com.brideside.crm.repository.InvitationTokenRepository;
import com.brideside.crm.repository.OrganizationRepository;
import com.brideside.crm.repository.PageAccessRepository;
import com.brideside.crm.repository.PersonRepository;
import com.brideside.crm.repository.RoleRepository;
import com.brideside.crm.repository.TeamRepository;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.EmailService;
import com.brideside.crm.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class UserServiceImpl implements UserService {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private RoleRepository roleRepository;

    @Autowired
    private OrganizationRepository organizationRepository;

    @Autowired
    private PersonRepository personRepository;

    @Autowired
    private TeamRepository teamRepository;

    @Autowired
    private InvitationTokenRepository invitationTokenRepository;

    @Autowired
    private EmailService emailService;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @Autowired
    private com.brideside.crm.repository.PasswordResetTokenRepository passwordResetTokenRepository;

    @Autowired
    private PageAccessRepository pageAccessRepository;

    @Value("${app.invitation-token-validity:604800000}")
    private long invitationTokenValidity;

    @Value("${app.frontend-url:http://localhost:3000}")
    private String frontendUrl;

    @Override
    @Transactional
    public UserResponse createUser(CreateUserRequest request) {
        if (userRepository.existsByEmail(request.getEmail())) {
            throw new BadRequestException("User with email " + request.getEmail() + " already exists");
        }

        Role.RoleName roleName;
        try {
            roleName = Role.RoleName.valueOf(request.getRole().toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new BadRequestException("Invalid role: " + request.getRole());
        }

        Role role = roleRepository.findByName(roleName)
                .orElseThrow(() -> new ResourceNotFoundException("Role not found: " + request.getRole()));

        User user = new User();
        user.setEmail(request.getEmail());
        user.setFirstName(request.getFirstName());
        user.setLastName(request.getLastName());
        user.setRole(role);
        user.setActive(false); // User will be activated after accepting invitation
        user.setPasswordSet(false);
        user.setPassword("TEMPORARY_PASSWORD"); // Temporary, will be set when user accepts invitation

        user.setManager(resolveManagerForRole(roleName, request.getManagerId(), null));

        user = userRepository.save(user);

        // Create invitation token
        InvitationToken invitationToken = InvitationToken.create(user, 
                (int) (invitationTokenValidity / (24 * 60 * 60 * 1000))); // Convert to days
        invitationTokenRepository.save(invitationToken);

        // Send invitation email
        emailService.sendInvitationEmail(user, invitationToken.getToken());

        return convertToResponse(user);
    }

    @Override
    @Transactional
    public void setPassword(SetPasswordRequest request) {
        // Validate passwords match
        if (!request.getPassword().equals(request.getConfirmPassword())) {
            throw new BadRequestException("Passwords do not match");
        }

        InvitationToken invitationToken = invitationTokenRepository
                .findByTokenAndUsedFalse(request.getToken())
                .orElseThrow(() -> new BadRequestException("Invalid or expired invitation token"));

        if (invitationToken.getExpiresAt().isBefore(LocalDateTime.now())) {
            throw new BadRequestException("Invitation token has expired");
        }

        if (invitationToken.getUsed()) {
            throw new BadRequestException("Invitation token has already been used");
        }

        User user = invitationToken.getUser();

        // Set password (encrypt it)
        user.setPassword(passwordEncoder.encode(request.getPassword()));
        user.setActive(true);
        user.setPasswordSet(true);

        // Mark token as used
        invitationToken.setUsed(true);

        userRepository.save(user);
        invitationTokenRepository.save(invitationToken);
    }

    @Override
    public UserResponse getUserById(Long id, String currentUserEmail) {
        User requestedUser = userRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with id: " + id));
        
        User currentUser = getCurrentUser(currentUserEmail);
        
        // Check if current user has access to view this user
        if (!canAccessUser(currentUser, requestedUser)) {
            throw new com.brideside.crm.exception.ForbiddenException("You don't have permission to view this user");
        }
        
        return convertToResponse(requestedUser);
    }

    @Override
    public List<UserResponse> getAllUsers(String currentUserEmail) {
        User currentUser = getCurrentUser(currentUserEmail);

        // Special handling for PRESALES:
        // For Activities page filters, frontend expects:
        // - the logged‑in Presales user
        // - their Sales manager (user with id = presales.managerId)
        if (currentUser.getRole() != null
                && currentUser.getRole().getName() == Role.RoleName.PRESALES) {
            // Always include the Presales user themselves
            List<User> scopedUsers = new java.util.ArrayList<>();
            scopedUsers.add(currentUser);

            // Include their Sales manager if configured
            User manager = currentUser.getManager();
            if (manager != null) {
                scopedUsers.add(manager);
            }

            return scopedUsers.stream()
                    .map(this::convertToResponse)
                    .collect(Collectors.toList());
        }

        // Check if user has access to "users" page
        // If CATEGORY_MANAGER or other roles have "users" page access, they can see all users like ADMIN
        boolean hasUsersPageAccess = hasPageAccess(currentUser, PageName.USERS);
        
        // Admin always has access, or if user has "users" page access enabled
        if (currentUser.getRole().getName() == Role.RoleName.ADMIN || hasUsersPageAccess) {
            // Return all users without filtering
            return userRepository.findAll().stream()
                    .map(this::convertToResponse)
                    .collect(Collectors.toList());
        }

        List<User> allUsers = userRepository.findAll();

        // Filter users based on role hierarchy
        List<User> accessibleUsers = allUsers.stream()
                .filter(user -> canAccessUser(currentUser, user))
                .collect(Collectors.toList());

        return accessibleUsers.stream()
                .map(this::convertToResponse)
                .collect(Collectors.toList());
    }

    @Override
    public void updateLastLogin(String email) {
        User user = userRepository.findByEmail(email)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with email: " + email));
        user.setLastLoginAt(LocalDateTime.now());
        userRepository.save(user);
    }

    @Override
    @Transactional
    public UserResponse updateUser(Long id, UpdateUserRequest request) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with id: " + id));

        // Check if email is being changed and if it already exists
        if (!user.getEmail().equals(request.getEmail()) && userRepository.existsByEmail(request.getEmail())) {
            throw new BadRequestException("User with email " + request.getEmail() + " already exists");
        }

        // Validate role
        Role.RoleName roleName;
        try {
            roleName = Role.RoleName.valueOf(request.getRole().toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new BadRequestException("Invalid role: " + request.getRole());
        }

        Role role = roleRepository.findByName(roleName)
                .orElseThrow(() -> new ResourceNotFoundException("Role not found: " + request.getRole()));

        // Update user fields
        user.setEmail(request.getEmail());
        user.setFirstName(request.getFirstName());
        user.setLastName(request.getLastName());
        user.setRole(role);

        User newManager = resolveManagerForRole(roleName, request.getManagerId(), user.getManager());
        if (newManager != null) {
            if (newManager.getId().equals(id)) {
                throw new BadRequestException("User cannot be their own manager");
            }
            if (isCircularManagerRelationship(id, newManager.getId())) {
                throw new BadRequestException("Cannot set manager: this would create a circular manager relationship");
            }
        }
        user.setManager(newManager);

        user = userRepository.save(user);
        return convertToResponse(user);
    }

    /**
     * Check if setting newManagerId as manager of userId would create a circular relationship
     */
    private boolean isCircularManagerRelationship(Long userId, Long newManagerId) {
        // If the new manager is the user itself, it's circular
        if (userId.equals(newManagerId)) {
            return true;
        }
        
        // Check if the new manager has userId as a manager in their hierarchy
        User newManager = userRepository.findById(newManagerId).orElse(null);
        if (newManager == null) {
            return false;
        }
        
        User current = newManager;
        int depth = 0;
        while (current.getManager() != null && depth < 100) { // Safety limit to prevent infinite loops
            if (current.getManager().getId().equals(userId)) {
                return true; // Circular relationship found
            }
            current = current.getManager();
            depth++;
        }
        
        return false;
    }

    @Override
    @Transactional
    public void deleteUser(Long id, Long reassignManagerId) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with id: " + id));

        // Check if user has any subordinates (users with this user as manager)
        User newManager = null;

        if (reassignManagerId != null) {
            if (reassignManagerId.equals(id)) {
                throw new BadRequestException("Cannot reassign subordinates to the user being deleted");
            }
            newManager = userRepository.findById(reassignManagerId)
                    .orElseThrow(() -> new ResourceNotFoundException("Manager not found with id: " + reassignManagerId));
        }

        List<User> subordinates = userRepository.findByManagerId(id);
        if (!subordinates.isEmpty()) {
            if (newManager == null) {
                throw new BadRequestException("Cannot delete user. This user has " + subordinates.size()
                        + " subordinate(s). Please provide a manager ID to reassign them to, or delete the subordinates first.");
            }

            for (User subordinate : subordinates) {
                subordinate.setManager(newManager);
                userRepository.save(subordinate);
            }
        }

        // Reassign or clear organization ownerships
        List<Organization> ownedOrganizations = organizationRepository.findByOwner_Id(id);
        for (Organization organization : ownedOrganizations) {
            if (newManager != null && newManager.getRole() != null &&
                    (newManager.getRole().getName() == Role.RoleName.SALES
                            || newManager.getRole().getName() == Role.RoleName.CATEGORY_MANAGER)) {
                organization.setOwner(newManager);
            } else {
                organization.setOwner(null);
            }
            organizationRepository.save(organization);
        }

        // Reassign or clear person ownerships (skip soft-deleted persons)
        List<Person> ownedPersons = personRepository.findByOwner_Id(id);
        for (Person person : ownedPersons) {
            // Skip soft-deleted persons
            if (Boolean.TRUE.equals(person.getIsDeleted())) {
                continue;
            }
            if (newManager != null && newManager.getRole() != null &&
                    newManager.getRole().getName() == Role.RoleName.SALES) {
                person.setOwner(newManager);
            } else {
                person.setOwner(null);
            }
            personRepository.save(person);
        }

        // Reassign team management
        List<Team> managedTeams = teamRepository.findByManager_Id(id);
        for (Team team : managedTeams) {
            if (newManager != null && newManager.getRole() != null &&
                    newManager.getRole().getName() == Role.RoleName.SALES) {
                team.setManager(newManager);
                team.getMembers().remove(user);
                team.getMembers().add(newManager);
            } else {
                team.setManager(null);
                team.getMembers().remove(user);
            }
            teamRepository.save(team);
        }

        // Remove from team memberships
        List<Team> memberTeams = teamRepository.findByMembers_Id(id);
        for (Team team : memberTeams) {
            if (team.getMembers().remove(user)) {
                teamRepository.save(team);
            }
        }

        // Delete related invitation tokens
        invitationTokenRepository.deleteByUserId(id);

        // Delete related password reset tokens
        passwordResetTokenRepository.deleteByUserId(id);

        // Delete the user
        userRepository.delete(user);
    }

    @Override
    @Transactional(readOnly = true)
    public List<TeamDtos.UserSummary> managerOptions(Role.RoleName roleName) {
        return getManagerCandidates(roleName).stream()
                .map(TeamDtos::toSummary)
                .collect(Collectors.toList());
    }
    
    /**
     * Check if current user can access target user based on role hierarchy
     */
    private boolean canAccessUser(User currentUser, User targetUser) {
        Role.RoleName currentRole = currentUser.getRole().getName();
        Role.RoleName targetRole = targetUser.getRole().getName();
        
        // Admin can see everyone
        if (currentRole == Role.RoleName.ADMIN) {
            return true;
        }
        
        // Users can always see themselves
        if (currentUser.getId().equals(targetUser.getId())) {
            return true;
        }
        
        // Category Manager can see all Sales and Presales users
        if (currentRole == Role.RoleName.CATEGORY_MANAGER) {
            if (targetRole == Role.RoleName.SALES || targetRole == Role.RoleName.PRESALES) {
                return true;  // Category Manager can see all SALES and PRESALES users
            }
            return false;
        }
        
        // Sales can see Presales under them
        if (currentRole == Role.RoleName.SALES) {
            if (targetRole == Role.RoleName.PRESALES) {
                return isUnderUser(currentUser, targetUser);
            }
            return false;
        }
        
        // Presales can only see themselves (already handled above)
        return false;
    }
    
    /**
     * Check if target user is under current user in the hierarchy
     */
    private boolean isUnderUser(User currentUser, User targetUser) {
        // Direct manager relationship
        if (targetUser.getManager() != null && targetUser.getManager().getId().equals(currentUser.getId())) {
            return true;
        }
        
        // For Category Manager, check if target's manager is under this Category Manager
        if (currentUser.getRole().getName() == Role.RoleName.CATEGORY_MANAGER) {
            if (targetUser.getManager() != null) {
                User targetManager = targetUser.getManager();
                // If target's manager is a Sales and reports to this Category Manager
                if (targetManager.getRole().getName() == Role.RoleName.SALES &&
                    targetManager.getManager() != null &&
                    targetManager.getManager().getId().equals(currentUser.getId())) {
                    return true;
                }
            }
        }
        
        return false;
    }
    
    /**
     * Get current authenticated user
     */
    private User getCurrentUser(String email) {
        return userRepository.findByEmail(email)
                .orElseThrow(() -> new ResourceNotFoundException("Current user not found"));
    }

    /**
     * Check if user has access to a specific page
     * Returns true if:
     * 1. User is ADMIN (always has access)
     * 2. There's a page_access record with has_access = true
     * 3. No record exists but role-based default grants access
     */
    private boolean hasPageAccess(User user, String pageName) {
        // Admin always has access
        if (user.getRole().getName() == Role.RoleName.ADMIN) {
            return true;
        }

        // Check if there's an explicit page access record
        return pageAccessRepository.findByUserIdAndPageName(user.getId(), pageName)
                .map(pageAccess -> pageAccess.getHasAccess())
                .orElseGet(() -> {
                    // No record exists, check role-based defaults
                    // For "users" page, default is false for all roles except ADMIN
                    // But we can check if role-based default would grant access
                    Role.RoleName roleName = user.getRole().getName();
                    if (pageName.equals(PageName.USERS)) {
                        // Only ADMIN has default access to users page
                        return false;
                    }
                    // For other pages, you could add role-based default logic here if needed
                    return false;
                });
    }

    private User resolveManagerForRole(Role.RoleName roleName, Long managerId, User existingManager) {
        Set<Role.RoleName> allowedRoles = getAllowedManagerRoles(roleName);
        if (allowedRoles.isEmpty()) {
            if (managerId != null) {
                throw new BadRequestException("Selected role does not support assigning a manager");
            }
            return null;
        }
        if (managerId == null) {
            if (existingManager != null && existingManager.getRole() != null &&
                    allowedRoles.contains(existingManager.getRole().getName())) {
                return existingManager;
            }
            return null;
        }
        User manager = userRepository.findById(managerId)
                .orElseThrow(() -> new ResourceNotFoundException("Manager not found with id: " + managerId));
        if (Boolean.FALSE.equals(manager.getActive())) {
            throw new BadRequestException("Manager must be an active user");
        }
        if (manager.getRole() == null || !allowedRoles.contains(manager.getRole().getName())) {
            throw new BadRequestException("Manager role is not permitted for selected user role");
        }
        return manager;
    }

    private Set<Role.RoleName> getAllowedManagerRoles(Role.RoleName roleName) {
        if (roleName == null) {
            return Set.of(Role.RoleName.CATEGORY_MANAGER, Role.RoleName.SALES);
        }
        switch (roleName) {
            case ADMIN:
            case CATEGORY_MANAGER:
                return Set.of();
            case SALES:
                return Set.of(Role.RoleName.CATEGORY_MANAGER);
            case PRESALES:
                return Set.of(Role.RoleName.SALES);
            default:
                return Set.of();
        }
    }

    private List<User> getManagerCandidates(Role.RoleName roleName) {
        Set<Role.RoleName> allowedRoles = getAllowedManagerRoles(roleName);
        if (allowedRoles.isEmpty()) {
            return List.of();
        }
        return userRepository.findByRole_NameInAndActiveTrue(allowedRoles);
    }

    private UserResponse convertToResponse(User user) {
        UserResponse response = new UserResponse();
        response.setId(user.getId());
        response.setEmail(user.getEmail());
        response.setFirstName(user.getFirstName());
        response.setLastName(user.getLastName());
        response.setRole(user.getRole().getName().name());
        response.setActive(user.getActive());
        response.setPasswordSet(user.getPasswordSet());
        response.setCreatedAt(user.getCreatedAt());
        response.setLastLoginAt(user.getLastLoginAt());
        // Set manager ID and name if exists
        if (user.getManager() != null) {
            response.setManagerId(user.getManager().getId());
            response.setManagerName(user.getManager().getFirstName() + " " + user.getManager().getLastName());
        }
        return response;
    }
}

