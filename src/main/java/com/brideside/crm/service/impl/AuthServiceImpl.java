package com.brideside.crm.service.impl;

import com.brideside.crm.dto.CreateAdminRequest;
import com.brideside.crm.dto.ForgotPasswordRequest;
import com.brideside.crm.dto.LoginRequest;
import com.brideside.crm.dto.LoginResponse;
import com.brideside.crm.dto.ResetPasswordRequest;
import com.brideside.crm.dto.UserResponse;
import com.brideside.crm.entity.PasswordResetToken;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.User;
import com.brideside.crm.exception.BadRequestException;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.exception.UnauthorizedException;
import com.brideside.crm.repository.PasswordResetTokenRepository;
import com.brideside.crm.repository.RoleRepository;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.EmailService;
import com.brideside.crm.security.JwtTokenProvider;
import com.brideside.crm.service.AuthService;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;

@Service
public class AuthServiceImpl implements AuthService {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private RoleRepository roleRepository;

    @Autowired
    private PasswordEncoder passwordEncoder;

    @Autowired
    private AuthenticationManager authenticationManager;

    @Autowired
    private JwtTokenProvider tokenProvider;
    
    @Autowired
    private PasswordResetTokenRepository passwordResetTokenRepository;
    
    @Autowired
    private EmailService emailService;
    
    @Value("${app.password-reset-token-validity:24}")
    private int passwordResetTokenValidityHours;
    
    @PersistenceContext
    private EntityManager entityManager;

    @Override
    @Transactional
    public UserResponse createAdminUser(CreateAdminRequest request) {
        // Check if admin already exists
        if (userRepository.existsByEmail(request.getEmail())) {
            throw new BadRequestException("Admin user with email " + request.getEmail() + " already exists");
        }

        // Check if admin already exists
        if (userRepository.findAll().stream()
                .anyMatch(u -> u.getRole().getName() == Role.RoleName.ADMIN)) {
            throw new BadRequestException("Admin user already exists. Please use the login endpoint.");
        }

        // Get or create admin role (database constraint handling - not business logic)
        Role adminRole = roleRepository.findByName(Role.RoleName.ADMIN)
                .orElseGet(() -> createRoleWithDatabaseConstraint("ADMIN", 
                    "Administrator role with full access. Can create, update, delete any data and see anyone's data."));

        // Create admin user
        User admin = new User();
        admin.setEmail(request.getEmail());
        admin.setPassword(passwordEncoder.encode(request.getPassword()));
        admin.setFirstName(request.getFirstName());
        admin.setLastName(request.getLastName());
        admin.setRole(adminRole);
        admin.setActive(true);
        admin.setPasswordSet(true);
        admin = userRepository.save(admin);

        return convertToResponse(admin);
    }
    
    private Role createRoleWithDatabaseConstraint(String roleName, String description) {
        // Create role using native SQL (no company_id needed)
        try {
            entityManager.createNativeQuery(
                "INSERT INTO roles (name, description) VALUES (?, ?)")
                .setParameter(1, roleName)
                .setParameter(2, description)
                .executeUpdate();
            
            entityManager.flush();
            
            return roleRepository.findByName(Role.RoleName.valueOf(roleName))
                    .orElseThrow(() -> new BadRequestException("Failed to create role: " + roleName));
        } catch (Exception e) {
            throw new BadRequestException("Failed to create role: " + e.getMessage());
        }
    }

    @Override
    public LoginResponse login(LoginRequest request) {
        try {
            // First check if user exists and is active before attempting authentication
            User user = userRepository.findByEmail(request.getEmail())
                    .orElseThrow(() -> new UnauthorizedException("Invalid email or password"));

            if (!user.getActive()) {
                throw new UnauthorizedException("Your account is not active. Please check your email and complete the registration process.");
            }

            if (!user.getPasswordSet()) {
                throw new UnauthorizedException("Please set your password first using the invitation link sent to your email.");
            }

            Authentication authentication = authenticationManager.authenticate(
                    new UsernamePasswordAuthenticationToken(request.getEmail(), request.getPassword())
            );

            SecurityContextHolder.getContext().setAuthentication(authentication);
            String token = tokenProvider.generateToken(authentication);

            // Update last login
            user.setLastLoginAt(LocalDateTime.now());
            userRepository.save(user);

            LoginResponse response = new LoginResponse();
            response.setToken(token);
            response.setUserId(user.getId());
            response.setEmail(user.getEmail());
            response.setFirstName(user.getFirstName());
            response.setLastName(user.getLastName());
            response.setRole(user.getRole().getName().name());

            return response;
        } catch (org.springframework.security.authentication.BadCredentialsException e) {
            throw new UnauthorizedException("Invalid email or password");
        } catch (org.springframework.security.core.userdetails.UsernameNotFoundException e) {
            throw new UnauthorizedException("Invalid email or password");
        }
    }

    @Override
    @Transactional
    public void activateAdminUser(String email) {
        User user = userRepository.findByEmail(email)
                .orElseThrow(() -> new ResourceNotFoundException("User not found with email: " + email));
        
        if (user.getRole().getName() != Role.RoleName.ADMIN) {
            throw new BadRequestException("User is not an admin. Only admin users can be activated via this endpoint.");
        }
        
        user.setActive(true);
        user.setPasswordSet(true);
        userRepository.save(user);
    }

    @Override
    @Transactional
    public void forgotPassword(ForgotPasswordRequest request) {
        User user = userRepository.findByEmail(request.getEmail())
                .orElseThrow(() -> new ResourceNotFoundException("User with email " + request.getEmail() + " not found"));

        // Check if user has set their password (active user)
        if (!user.getPasswordSet()) {
            throw new BadRequestException("Please complete your initial password setup first");
        }

        // Invalidate any existing unused reset tokens for this user
        // Note: We'll mark old tokens as used when creating new ones
        // For simplicity, we just create a new token each time

        // Create new password reset token
        PasswordResetToken resetToken = PasswordResetToken.create(user, passwordResetTokenValidityHours);
        passwordResetTokenRepository.save(resetToken);

        // Send password reset email
        emailService.sendPasswordResetEmail(user, resetToken.getToken());
    }

    @Override
    @Transactional
    public void resetPassword(ResetPasswordRequest request) {
        // Validate passwords match
        if (!request.getPassword().equals(request.getConfirmPassword())) {
            throw new BadRequestException("Passwords do not match");
        }

        PasswordResetToken resetToken = passwordResetTokenRepository
                .findByTokenAndUsedFalse(request.getToken())
                .orElseThrow(() -> new BadRequestException("Invalid or expired password reset token"));

        if (resetToken.getExpiresAt().isBefore(LocalDateTime.now())) {
            throw new BadRequestException("Password reset token has expired");
        }

        if (resetToken.getUsed()) {
            throw new BadRequestException("Password reset token has already been used");
        }

        User user = resetToken.getUser();

        // Set new password (encrypt it)
        user.setPassword(passwordEncoder.encode(request.getPassword()));
        user.setPasswordSet(true);

        // Mark token as used
        resetToken.setUsed(true);

        userRepository.save(user);
        passwordResetTokenRepository.save(resetToken);
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
        if (user.getManager() != null) {
            response.setManagerName(user.getManager().getFirstName() + " " + user.getManager().getLastName());
        }
        return response;
    }
}

