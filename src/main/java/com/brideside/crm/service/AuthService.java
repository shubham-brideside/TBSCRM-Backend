package com.brideside.crm.service;

import com.brideside.crm.dto.CreateAdminRequest;
import com.brideside.crm.dto.ForgotPasswordRequest;
import com.brideside.crm.dto.LoginRequest;
import com.brideside.crm.dto.LoginResponse;
import com.brideside.crm.dto.ResetPasswordRequest;
import com.brideside.crm.dto.UserResponse;

public interface AuthService {
    UserResponse createAdminUser(CreateAdminRequest request);
    LoginResponse login(LoginRequest request);
    void forgotPassword(ForgotPasswordRequest request);
    void resetPassword(ResetPasswordRequest request);
    void activateAdminUser(String email);
}
