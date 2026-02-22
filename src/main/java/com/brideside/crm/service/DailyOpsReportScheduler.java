package com.brideside.crm.service;

import jakarta.annotation.PostConstruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class DailyOpsReportScheduler {

    private static final Logger log = LoggerFactory.getLogger(DailyOpsReportScheduler.class);

    private final DailyOpsReportService reportService;

    @Value("${app.reports.daily.enabled:false}")
    private boolean enabled;

    @Value("${app.reports.daily.recipients:}")
    private String recipients;

    @Value("${app.reports.timezone:Asia/Kolkata}")
    private String timezone;

    @Value("${app.reports.daily.cron:0 0 9 * * *}")
    private String cron;

    public DailyOpsReportScheduler(DailyOpsReportService reportService) {
        this.reportService = reportService;
    }

    @PostConstruct
    public void logConfig() {
        List<String> emails = parseRecipients(recipients);
        log.info("Daily ops report scheduler config: enabled={}, zone={}, cron='{}', recipientsCount={}, recipients={}",
                enabled, timezone, cron, emails.size(), emails);
    }

    @Scheduled(cron = "${app.reports.daily.cron:0 0 9 * * *}", zone = "${app.reports.timezone:Asia/Kolkata}")
    public void sendDailyReport() {
        if (!enabled) {
            return;
        }
        List<String> emails = parseRecipients(recipients);
        if (emails.isEmpty()) {
            log.warn("Daily ops report enabled but no recipients configured");
            return;
        }

        log.info("Sending daily ops report to {} recipients (zone={})", emails.size(), timezone);
        for (String to : emails) {
            try {
                reportService.sendDailyReportTo(to);
            } catch (Exception e) {
                log.error("Failed to send daily ops report to {}", to, e);
            }
        }
    }

    private static List<String> parseRecipients(String raw) {
        if (raw == null || raw.isBlank()) return List.of();
        return Arrays.stream(raw.split(","))
                .map(String::trim)
                .filter(s -> !s.isEmpty())
                .distinct()
                .collect(Collectors.toList());
    }
}

