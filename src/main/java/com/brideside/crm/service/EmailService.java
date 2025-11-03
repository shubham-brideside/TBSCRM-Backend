package com.brideside.crm.service;

import com.brideside.crm.entity.User;

public interface EmailService {
    void sendInvitationEmail(User user, String token);
    void sendPasswordResetEmail(User user, String token);
    void sendTestEmail(String toEmail, String subject, String message);
}
