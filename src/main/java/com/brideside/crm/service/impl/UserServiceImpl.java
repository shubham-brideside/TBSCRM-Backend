package com.brideside.crm.service.impl;

import com.brideside.crm.dto.CreateUserRequest;
import com.brideside.crm.dto.SetPasswordRequest;
import com.brideside.crm.dto.UpdateUserRequest;
import com.brideside.crm.dto.UserResponse;
import com.brideside.crm.entity.InvitationToken;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.InvitationTokenRepository;
import com.brideside.crm.repository.RoleRepository;
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
import java.util.stream.Collectors;

@Service
public class UserServiceImpl implements UserService {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private RoleRepository roleRepository;

    @Autowired
    private InvitationTokenRepository invitationTokenRepository;

    @Autowired
    private EmailService emailService;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @Autowired
    private com.brideside.crm.repository.PasswordResetTokenRepository passwordResetTokenRepository;

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

        // Set manager if provided
        if (request.getManagerId() != null) {
            User manager = userRepository.findById(request.getManagerId())
                    .orElseThrow(() -> new ResourceNotFoundException("Manager not found with id: " + request.getManagerId()));
            user.setManager(manager);
        }

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

        // Update manager if provided
        if (request.getManagerId() != null) {
            // Prevent user from being their own manager
            if (request.getManagerId().equals(id)) {
                throw new BadRequestException("User cannot be their own manager");
            }
            
            User manager = userRepository.findById(request.getManagerId())
                    .orElseThrow(() -> new ResourceNotFoundException("Manager not found with id: " + request.getManagerId()));
            
            // Prevent circular manager relationships
            if (isCircularManagerRelationship(id, manager.getId())) {
                throw new BadRequestException("Cannot set manager: this would create a circular manager relationship");
            }
            
            user.setManager(manager);
        } else {
            user.setManager(null);
        }

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
        List<User> subordinates = userRepository.findByManagerId(id);
        if (!subordinates.isEmpty()) {
            // If reassignManagerId is provided, reassign subordinates to new manager
            if (reassignManagerId != null) {
                User newManager = userRepository.findById(reassignManagerId)
                        .orElseThrow(() -> new ResourceNotFoundException("Manager not found with id: " + reassignManagerId));
                
                // Validate that new manager is not the user being deleted
                if (reassignManagerId.equals(id)) {
                    throw new BadRequestException("Cannot reassign subordinates to the user being deleted");
                }
                
                // Reassign all subordinates to the new manager
                for (User subordinate : subordinates) {
                    subordinate.setManager(newManager);
                    userRepository.save(subordinate);
                }
            } else {
                // No reassignment provided, throw error
                throw new BadRequestException("Cannot delete user. This user has " + subordinates.size() + 
                        " subordinate(s). Please provide a manager ID to reassign them to, or delete the subordinates first.");
            }
        }

        // Delete related invitation tokens
        invitationTokenRepository.deleteByUserId(id);

        // Delete related password reset tokens
        passwordResetTokenRepository.deleteByUserId(id);

        // Delete the user
        userRepository.delete(user);
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
        
        // Category Manager can see Managers and Salesrep under them
        if (currentRole == Role.RoleName.CATEGORY_MANAGER) {
            if (targetRole == Role.RoleName.MANAGER || targetRole == Role.RoleName.SALESREP) {
                return isUnderUser(currentUser, targetUser);
            }
            return false;
        }
        
        // Manager can see Salesrep under them
        if (currentRole == Role.RoleName.MANAGER) {
            if (targetRole == Role.RoleName.SALESREP) {
                return isUnderUser(currentUser, targetUser);
            }
            return false;
        }
        
        // Salesrep can only see themselves (already handled above)
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
                // If target's manager is a Manager and reports to this Category Manager
                if (targetManager.getRole().getName() == Role.RoleName.MANAGER &&
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

