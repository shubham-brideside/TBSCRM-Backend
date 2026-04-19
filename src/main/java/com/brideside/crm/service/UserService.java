package com.brideside.crm.service;

import com.brideside.crm.dto.CreateUserRequest;
import com.brideside.crm.dto.SetPasswordRequest;
import com.brideside.crm.dto.TeamDtos;
import com.brideside.crm.dto.PatchManagedCategoryRequest;
import com.brideside.crm.dto.UpdateUserRequest;
import com.brideside.crm.dto.UserResponse;
import com.brideside.crm.entity.Role;

import java.util.List;

public interface UserService {
    UserResponse createUser(CreateUserRequest request);
    void setPassword(SetPasswordRequest request);
    UserResponse getUserById(Long id, String currentUserEmail);
    List<UserResponse> getAllUsers(String currentUserEmail);
    void updateLastLogin(String email);
    UserResponse updateUser(Long id, UpdateUserRequest request);

    /**
     * Set or clear {@code users.user_managed_category_id} for a category manager.
     * Allowed for ADMIN (any user id) or for CATEGORY_MANAGER only when {@code userId} is their own id.
     */
    UserResponse patchManagedCategory(Long userId, PatchManagedCategoryRequest request, String callerEmail);

    /** Sets managed category for the authenticated user (same rules as {@link #patchManagedCategory} for "self"). */
    UserResponse patchMyManagedCategory(PatchManagedCategoryRequest request, String callerEmail);

    void deleteUser(Long id, Long reassignManagerId);
    List<TeamDtos.UserSummary> managerOptions(Role.RoleName roleName);
}
