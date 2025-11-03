package com.brideside.crm.service.impl;

import com.brideside.crm.entity.User;
import com.brideside.crm.service.EmailService;
import com.mailersend.sdk.MailerSend;
import com.mailersend.sdk.MailerSendResponse;
import com.mailersend.sdk.emails.Email;
import com.mailersend.sdk.exceptions.MailerSendException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import jakarta.annotation.PostConstruct;


@Service
public class EmailServiceImpl implements EmailService {

    private static final Logger logger = LoggerFactory.getLogger(EmailServiceImpl.class);
    
    private MailerSend mailerSend;

    @Value("${mailersend.api-token:}")
    private String mailerSendApiToken;

    @Value("${mailersend.from-email:noreply@bridesidecrm.com}")
    private String fromEmail;

    @Value("${mailersend.from-name:Brideside CRM}")
    private String fromName;

    @Value("${app.frontend-url:http://localhost:3000}")
    private String frontendUrl;

    @PostConstruct
    public void init() {
        if (mailerSendApiToken != null && !mailerSendApiToken.isEmpty()) {
            mailerSend = new MailerSend();
            mailerSend.setToken(mailerSendApiToken);
            logger.info("MailerSend SDK initialized successfully");
        } else {
            logger.warn("MailerSend API token is not configured. Email sending will be disabled.");
        }
    }

    @Override
    public void sendInvitationEmail(User user, String token) {
        if (mailerSend == null || mailerSendApiToken == null || mailerSendApiToken.isEmpty()) {
            logger.error("MailerSend API token is not configured. Cannot send invitation email to: {}", user.getEmail());
            return;
        }

        String subject = "Welcome to Brideside CRM - Complete Your Registration";
        String body = buildInvitationEmailBody(user, token);

        sendEmail(user.getEmail(), subject, body, false);
    }

    @Override
    public void sendPasswordResetEmail(User user, String token) {
        if (mailerSend == null || mailerSendApiToken == null || mailerSendApiToken.isEmpty()) {
            logger.error("MailerSend API token is not configured. Cannot send password reset email to: {}", user.getEmail());
            return;
        }

        String subject = "Brideside CRM - Password Reset Request";
        String body = buildPasswordResetEmailBody(user, token);

        sendEmail(user.getEmail(), subject, body, false);
    }

    @Override
    public void sendTestEmail(String toEmail, String subject, String message) {
        if (mailerSend == null || mailerSendApiToken == null || mailerSendApiToken.isEmpty()) {
            logger.error("MailerSend API token is not configured. Cannot send test email to: {}", toEmail);
            throw new RuntimeException("MailerSend API token is not configured");
        }

        sendEmail(toEmail, subject, message, true);
    }

    private void sendEmail(String toEmail, String subject, String text, boolean throwOnError) {
        try {
            Email email = new Email();
            
            // Set from address
            email.setFrom(fromName, fromEmail);
            
            // Add recipient
            email.addRecipient(null, toEmail);
            
            // Set subject
            email.setSubject(subject);
            
            // Set plain text content
            email.setPlain(text);
            
            // Send email using MailerSend SDK
            MailerSendResponse response = mailerSend.emails().send(email);
            
            logger.info("Email sent successfully to: {}. Message ID: {}", toEmail, response.messageId);
        } catch (MailerSendException e) {
            String errorMessage = "Failed to send email to: " + toEmail;
            logger.error(errorMessage, e);
            
            if (e.getMessage() != null && e.getMessage().contains("domain must be verified")) {
                logger.error("Domain verification issue:");
                logger.error("1. The 'from.email' domain must be verified in your MailerSend account");
                logger.error("2. Check your MailerSend dashboard to verify the domain: {}", fromEmail.split("@")[1]);
                logger.error("3. Or use a verified email address in application.yml (mailersend.from-email)");
            }
            
            if (throwOnError) {
                throw new RuntimeException("Failed to send email: " + e.getMessage(), e);
            }
            // For regular emails (invitation, password reset), log but don't throw
            // This ensures user creation/reset doesn't fail if email service has issues
        } catch (Exception e) {
            logger.error("Failed to send email to: {}", toEmail, e);
            if (throwOnError) {
                throw new RuntimeException("Failed to send email: " + e.getMessage(), e);
            }
            // For regular emails (invitation, password reset), log but don't throw
            // This ensures user creation/reset doesn't fail if email service has issues
        }
    }

    private String buildInvitationEmailBody(User user, String token) {
        String invitationLink = frontendUrl + "/accept-invitation?token=" + token;
        
        return "Dear " + user.getFirstName() + " " + user.getLastName() + ",\n\n" +
                "You have been invited to join Brideside CRM.\n\n" +
                "Please click on the link below to accept the invitation and set your password:\n" +
                invitationLink + "\n\n" +
                "This link will expire in 7 days.\n\n" +
                "If you did not request this invitation, please ignore this email.\n\n" +
                "Best regards,\n" +
                "Brideside CRM Team";
    }

    private String buildPasswordResetEmailBody(User user, String token) {
        String resetLink = frontendUrl + "/reset-password?token=" + token;
        
        return "Dear " + user.getFirstName() + " " + user.getLastName() + ",\n\n" +
                "We received a request to reset your password for your Brideside CRM account.\n\n" +
                "Please click on the link below to reset your password:\n" +
                resetLink + "\n\n" +
                "This link will expire in 24 hours.\n\n" +
                "If you did not request a password reset, please ignore this email. Your password will remain unchanged.\n\n" +
                "Best regards,\n" +
                "Brideside CRM Team";
    }
}

