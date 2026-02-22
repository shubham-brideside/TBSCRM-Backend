package com.brideside.crm.service;

import com.brideside.crm.entity.User;

public interface EmailService {
    void sendInvitationEmail(User user, String token);
    void sendPasswordResetEmail(User user, String token);
    void sendTestEmail(String toEmail, String subject, String message);
    void sendHtmlEmail(String toEmail, String subject, String html, String plainTextFallback);
}
