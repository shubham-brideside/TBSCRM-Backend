package com.brideside.crm.service.impl;

import com.brideside.crm.entity.User;
import com.brideside.crm.service.EmailService;
import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;

@Service
public class EmailServiceImpl implements EmailService {

    private static final Logger logger = LoggerFactory.getLogger(EmailServiceImpl.class);

    @Autowired
    private JavaMailSender mailSender;

    @Value("${app.email.from:shubhamlohra35@gmail.com}")
    private String fromEmail;

    @Value("${app.email.from-name:Brideside CRM}")
    private String fromName;

    @Value("${app.frontend-url:http://localhost:3000}")
    private String frontendUrl;

    @Override
    public void sendInvitationEmail(User user, String token) {
        String subject = "Welcome to Brideside CRM - Complete Your Registration";
        String body = buildInvitationEmailBody(user, token);

        sendEmail(user.getEmail(), subject, body, false);
    }

    @Override
    public void sendPasswordResetEmail(User user, String token) {
        String subject = "Brideside CRM - Password Reset Request";
        String body = buildPasswordResetEmailBody(user, token);

        sendEmail(user.getEmail(), subject, body, false);
    }

    @Override
    public void sendTestEmail(String toEmail, String subject, String message) {
        sendEmail(toEmail, subject, message, true);
    }

    private void sendEmail(String toEmail, String subject, String text, boolean throwOnError) {
        try {
            MimeMessage mimeMessage = mailSender.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true, "UTF-8");

            // Set from address
            helper.setFrom(fromEmail, fromName);

            // Set to address
            helper.setTo(toEmail);

            // Set subject
            helper.setSubject(subject);

            // Set plain text content
            helper.setText(text, false);

            // Send email
            mailSender.send(mimeMessage);

            logger.info("Email sent successfully to: {}", toEmail);
        } catch (MessagingException e) {
            String errorMessage = "Failed to send email to: " + toEmail;
            logger.error(errorMessage, e);
            
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
        String invitationLink = frontendUrl + "/accept-invitation.html?token=" + token;
        
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
        String resetLink = frontendUrl + "/reset-password.html?token=" + token;
        
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
