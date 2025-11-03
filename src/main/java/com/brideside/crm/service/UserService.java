package com.brideside.crm.service;

import com.brideside.crm.dto.CreateUserRequest;
import com.brideside.crm.dto.SetPasswordRequest;
import com.brideside.crm.dto.UpdateUserRequest;
import com.brideside.crm.dto.UserResponse;

import java.util.List;

public interface UserService {
    UserResponse createUser(CreateUserRequest request);
    void setPassword(SetPasswordRequest request);
    UserResponse getUserById(Long id, String currentUserEmail);
    List<UserResponse> getAllUsers(String currentUserEmail);
    void updateLastLogin(String email);
    UserResponse updateUser(Long id, UpdateUserRequest request);
    void deleteUser(Long id, Long reassignManagerId);
}
