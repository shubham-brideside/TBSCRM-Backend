package com.brideside.crm.service;

import com.brideside.crm.entity.User;

import java.util.List;

public interface EmailService {
    void sendInvitationEmail(User user, String token);
    void sendPasswordResetEmail(User user, String token);
    void sendTestEmail(String toEmail, String subject, String message);
    void sendHtmlEmail(String toEmail, String subject, String html, String plainTextFallback);
    void sendHtmlEmailWithCc(String toEmail, List<String> ccEmails, String subject, String html, String plainTextFallback);
}
