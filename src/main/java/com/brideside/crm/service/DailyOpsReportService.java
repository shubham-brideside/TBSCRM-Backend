package com.brideside.crm.service;

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.StringJoiner;
import java.util.stream.Collectors;

@Service
public class DailyOpsReportService {

    private static final DateTimeFormatter PRETTY_DATE = DateTimeFormatter.ofPattern("dd MMM yyyy", Locale.ENGLISH);

    private final JdbcTemplate jdbcTemplate;
    private final EmailService emailService;
    private final ZoneId reportZone;

    private final int maxRowsPerTable;
    private final boolean hideZeroRows;

    public DailyOpsReportService(
            JdbcTemplate jdbcTemplate,
            EmailService emailService,
            org.springframework.core.env.Environment env
    ) {
        this.jdbcTemplate = jdbcTemplate;
        this.emailService = emailService;
        this.reportZone = ZoneId.of(env.getProperty("app.reports.timezone", "Asia/Kolkata"));
        this.maxRowsPerTable = Integer.parseInt(env.getProperty("app.reports.daily.maxRowsPerTable", "20"));
        this.hideZeroRows = Boolean.parseBoolean(env.getProperty("app.reports.daily.hideZeroRows", "true"));
    }

    public void sendDailyReportTo(String toEmail) {
        LocalDate reportDate = LocalDate.now(reportZone).minusDays(1);
        sendDailyReportTo(toEmail, reportDate);
    }

    public void sendDailyReportTo(String toEmail, LocalDate reportDate) {
        Objects.requireNonNull(toEmail, "toEmail");
        Objects.requireNonNull(reportDate, "reportDate");

        // IMPORTANT: use LocalDateTime for MySQL DATETIME comparisons to avoid timezone shifts
        // (DATETIME has no timezone; binding Timestamp can introduce conversion depending on JVM/driver timezone).
        LocalDateTime start = reportDate.atStartOfDay();
        LocalDateTime end = reportDate.plusDays(1).atStartOfDay();

        String subject = "Brideside CRM - Daily Ops Report (" + reportDate + ")";
        String plain = buildPlainText(reportDate, start, end);
        String html = buildHtml(reportDate, start, end);
        emailService.sendHtmlEmail(toEmail, subject, html, plain);
    }

    private String buildPlainText(LocalDate reportDate, LocalDateTime start, LocalDateTime end) {
        StringBuilder sb = new StringBuilder();
        sb.append("Hi\n\n");
        sb.append("Daily report for: ").append(reportDate).append("\n");
        sb.append("Window (local): ").append(start).append(" to ").append(end).append("\n\n");

        sb.append("1) Total number of new deals created per account\n");
        sb.append(formatOrgTable(queryNewDealsPerOrg(start, end), "new_deals")).append("\n\n");

        sb.append("2) Deals created by BOT vs manual by PreSales (per account)\n");
        sb.append(formatOrgMultiTable(queryBotVsManualPresalesPerOrg(start, end),
                List.of("bot_deals", "manual_presales_deals"))).append("\n\n");

        sb.append("3) Deals WON / LOST (per account)\n");
        sb.append(formatOrgMultiTable(queryWonLostPerOrg(start, end),
                List.of("won_count", "lost_count"))).append("\n\n");

        sb.append("4) Calls done by sales reps (count + minutes)\n");
        sb.append(formatUserCallsTable(queryCallsPerRep(start, end))).append("\n\n");

        sb.append("5) City wise distribution of leads coming in\n");
        sb.append(formatCityTable(queryCityDistribution(start, end))).append("\n\n");

        sb.append("6) Deals moved from Qualified to Contact Made (per account)\n");
        sb.append(formatOrgTable(queryMovedQualifiedToContactMadePerOrg(start, end), "moved_qualified_to_contact_made"))
                .append("\n\n");

        sb.append("7) Deals in Qualified (IN_PROGRESS) rotting for 2-3 days (per account)\n");
        sb.append(formatOrgTable(queryQualifiedRotting2to3DaysPerOrg(end), "rotting_2_to_3_days")).append("\n");

        return sb.toString();
    }

    private String buildHtml(LocalDate reportDate, LocalDateTime start, LocalDateTime end) {
        List<Map<String, Object>> newDeals = queryNewDealsPerOrg(start, end);
        List<Map<String, Object>> botVsManual = queryBotVsManualPresalesPerOrg(start, end);
        List<Map<String, Object>> wonLost = queryWonLostPerOrg(start, end);
        List<Map<String, Object>> calls = queryCallsPerRep(start, end);
        List<Map<String, Object>> cities = queryCityDistribution(start, end);
        List<Map<String, Object>> moved = queryMovedQualifiedToContactMadePerOrg(start, end);
        List<Map<String, Object>> rotting = queryQualifiedRotting2to3DaysPerOrg(end);

        long totalNewDeals = queryTotalNewDeals(start, end);
        long totalBotDeals = sumLong(botVsManual, "bot_deals");
        long totalManualPresales = sumLong(botVsManual, "manual_presales_deals");
        long totalWon = sumLong(wonLost, "won_count");
        long totalLost = sumLong(wonLost, "lost_count");
        long totalCalls = sumLong(calls, "calls_done");
        long totalCallMinutes = sumLong(calls, "total_call_minutes");

        String content = ""
                + sectionKpis(totalNewDeals, totalBotDeals, totalManualPresales, totalWon, totalLost, totalCalls, totalCallMinutes)
                + section("1) New deals created per account",
                "Total: " + fmt(totalNewDeals),
                htmlTable(newDeals,
                        List.of("organization_id", "organization_name", "new_deals"),
                        List.of("new_deals"),
                        hideZeroRows,
                        maxRowsPerTable))
                + section("2) BOT vs manual (PreSales) per account",
                "BOT: " + fmt(totalBotDeals) + " • Manual (PreSales): " + fmt(totalManualPresales),
                htmlTable(botVsManual,
                        List.of("organization_id", "organization_name", "bot_deals", "manual_presales_deals"),
                        List.of("bot_deals", "manual_presales_deals"),
                        hideZeroRows,
                        maxRowsPerTable))
                + section("3) Deals WON / LOST per account",
                "Won: " + fmt(totalWon) + " • Lost: " + fmt(totalLost),
                htmlTable(wonLost,
                        List.of("organization_id", "organization_name", "won_count", "lost_count"),
                        List.of("won_count", "lost_count"),
                        hideZeroRows,
                        maxRowsPerTable))
                + section("4) Calls done by sales reps",
                "Calls: " + fmt(totalCalls) + " • Minutes: " + fmt(totalCallMinutes),
                htmlTable(calls,
                        List.of("user_id", "user_name", "role_name", "calls_done", "total_call_minutes"),
                        List.of("calls_done", "total_call_minutes"),
                        hideZeroRows,
                        maxRowsPerTable))
                + section("5) City-wise distribution of leads",
                "Total new deals: " + fmt(totalNewDeals),
                htmlTable(cities,
                        List.of("city", "deals_created"),
                        List.of("deals_created"),
                        true,
                        maxRowsPerTable))
                + section("6) Deals moved Qualified → Contact Made (per account)",
                "Window: previous day only",
                htmlTable(moved,
                        List.of("organization_id", "organization_name", "moved_qualified_to_contact_made"),
                        List.of("moved_qualified_to_contact_made"),
                        hideZeroRows,
                        maxRowsPerTable))
                + section("7) Qualified rotting (IN_PROGRESS) 2–3 days (per account)",
                "As-of: end of previous day",
                htmlTable(rotting,
                        List.of("organization_id", "organization_name", "rotting_2_to_3_days"),
                        List.of("rotting_2_to_3_days"),
                        hideZeroRows,
                        maxRowsPerTable));

        return wrapHtml(reportDate, start, end, content);
    }

    private List<Map<String, Object>> queryNewDealsPerOrg(LocalDateTime start, LocalDateTime end) {
        String sql = """
                SELECT *
                FROM (
                  SELECT
                    o.id AS organization_id,
                    o.name AS organization_name,
                    COUNT(d.id) AS new_deals
                  FROM organizations o
                  LEFT JOIN deals d
                    ON d.organization_id = o.id
                   AND (d.is_deleted = 0 OR d.is_deleted IS NULL)
                   AND d.created_at >= ? AND d.created_at < ?
                  GROUP BY o.id, o.name

                  UNION ALL

                  SELECT
                    0 AS organization_id,
                    '(no organization)' AS organization_name,
                    COUNT(d2.id) AS new_deals
                  FROM deals d2
                  LEFT JOIN organizations o2 ON o2.id = d2.organization_id
                  WHERE (d2.is_deleted = 0 OR d2.is_deleted IS NULL)
                    AND d2.created_at >= ? AND d2.created_at < ?
                    AND o2.id IS NULL
                ) t
                ORDER BY new_deals DESC, organization_name ASC
                """;
        return jdbcTemplate.queryForList(sql, start, end, start, end);
    }

    private long queryTotalNewDeals(LocalDateTime start, LocalDateTime end) {
        String sql = """
                SELECT COUNT(*) AS total
                FROM deals d
                WHERE (d.is_deleted = 0 OR d.is_deleted IS NULL)
                  AND d.created_at >= ? AND d.created_at < ?
                """;
        Map<String, Object> row = jdbcTemplate.queryForMap(sql, start, end);
        Object v = row.get("total");
        if (v instanceof Number n) return n.longValue();
        return Long.parseLong(String.valueOf(v));
    }

    private List<Map<String, Object>> queryBotVsManualPresalesPerOrg(LocalDateTime start, LocalDateTime end) {
        String sql = """
                SELECT
                  o.id AS organization_id,
                  o.name AS organization_name,
                  COALESCE(SUM(CASE
                    WHEN d.id IS NOT NULL
                     AND (d.created_by = 'BOT' OR TRIM(UPPER(COALESCE(d.created_by_name,''))) = 'BOT')
                    THEN 1 ELSE 0 END), 0) AS bot_deals,
                  COALESCE(SUM(CASE
                    WHEN d.id IS NOT NULL
                     AND (d.created_by = 'USER' OR d.created_by IS NULL)
                     AND r.name = 'PRESALES'
                    THEN 1 ELSE 0 END), 0) AS manual_presales_deals
                FROM organizations o
                LEFT JOIN deals d
                  ON d.organization_id = o.id
                 AND (d.is_deleted = 0 OR d.is_deleted IS NULL)
                 AND d.created_at >= ? AND d.created_at < ?
                LEFT JOIN users u ON u.id = d.created_by_user_id
                LEFT JOIN roles r ON r.id = u.role_id
                GROUP BY o.id, o.name
                ORDER BY bot_deals DESC, manual_presales_deals DESC, o.name ASC
                """;
        return jdbcTemplate.queryForList(sql, start, end);
    }

    private List<Map<String, Object>> queryWonLostPerOrg(LocalDateTime start, LocalDateTime end) {
        String sql = """
                SELECT
                  o.id AS organization_id,
                  o.name AS organization_name,
                  COALESCE(SUM(CASE WHEN d.won_at  >= ? AND d.won_at  < ? THEN 1 ELSE 0 END), 0) AS won_count,
                  COALESCE(SUM(CASE WHEN d.lost_at >= ? AND d.lost_at < ? THEN 1 ELSE 0 END), 0) AS lost_count
                FROM organizations o
                LEFT JOIN deals d
                  ON d.organization_id = o.id
                 AND (d.is_deleted = 0 OR d.is_deleted IS NULL)
                 AND (
                      (d.won_at  >= ? AND d.won_at  < ?)
                   OR (d.lost_at >= ? AND d.lost_at < ?)
                 )
                GROUP BY o.id, o.name
                ORDER BY won_count DESC, lost_count DESC, o.name ASC
                """;
        return jdbcTemplate.queryForList(sql, start, end, start, end, start, end, start, end);
    }

    private List<Map<String, Object>> queryCallsPerRep(LocalDateTime start, LocalDateTime end) {
        String sql = """
                SELECT
                  u.id AS user_id,
                  CONCAT(COALESCE(u.first_name,''), ' ', COALESCE(u.last_name,'')) AS user_name,
                  r.name AS role_name,
                  COUNT(*) AS calls_done,
                  COALESCE(SUM(COALESCE(a.duration_minutes, 0)), 0) AS total_call_minutes
                FROM activities a
                JOIN users u ON u.id = COALESCE(a.assigned_user_id, a.user_id)
                JOIN roles r ON r.id = u.role_id
                WHERE a.done = 1
                  AND a.category = 'CALL'
                  AND a.completed_at >= ? AND a.completed_at < ?
                  AND r.name IN ('SALES', 'CATEGORY_MANAGER')
                GROUP BY u.id, u.first_name, u.last_name, r.name
                ORDER BY calls_done DESC, total_call_minutes DESC, user_name ASC
                """;
        return jdbcTemplate.queryForList(sql, start, end);
    }

    private List<Map<String, Object>> queryCityDistribution(LocalDateTime start, LocalDateTime end) {
        String sql = """
                SELECT
                  COALESCE(NULLIF(TRIM(d.city), ''), '(unknown)') AS city,
                  COUNT(*) AS deals_created
                FROM deals d
                WHERE (d.is_deleted = 0 OR d.is_deleted IS NULL)
                  AND d.created_at >= ? AND d.created_at < ?
                GROUP BY city
                ORDER BY deals_created DESC, city ASC
                """;
        return jdbcTemplate.queryForList(sql, start, end);
    }

    private List<Map<String, Object>> queryMovedQualifiedToContactMadePerOrg(LocalDateTime start, LocalDateTime end) {
        String sql = """
                WITH hist AS (
                  SELECT
                    h.deal_id,
                    h.entered_at,
                    s.name AS stage_name,
                    LAG(s.name) OVER (PARTITION BY h.deal_id ORDER BY h.entered_at, h.id) AS prev_stage_name
                  FROM deal_stage_history h
                  JOIN stages s ON s.id = h.stage_id
                ),
                moved AS (
                  SELECT DISTINCT deal_id
                  FROM hist
                  WHERE stage_name = 'Contact Made'
                    AND prev_stage_name = 'Qualified'
                    AND entered_at >= ? AND entered_at < ?
                )
                SELECT
                  o.id AS organization_id,
                  o.name AS organization_name,
                  COUNT(m.deal_id) AS moved_qualified_to_contact_made
                FROM organizations o
                LEFT JOIN deals d
                  ON d.organization_id = o.id
                 AND (d.is_deleted = 0 OR d.is_deleted IS NULL)
                LEFT JOIN moved m
                  ON m.deal_id = d.id
                GROUP BY o.id, o.name
                ORDER BY moved_qualified_to_contact_made DESC, o.name ASC
                """;
        return jdbcTemplate.queryForList(sql, start, end);
    }

    private List<Map<String, Object>> queryQualifiedRotting2to3DaysPerOrg(LocalDateTime asOf) {
        String sql = """
                SELECT
                  o.id AS organization_id,
                  o.name AS organization_name,
                  COALESCE(SUM(CASE
                    WHEN s.name = 'Qualified'
                     AND h.entered_at < DATE_SUB(?, INTERVAL 2 DAY)
                     AND h.entered_at >= DATE_SUB(?, INTERVAL 3 DAY)
                    THEN 1 ELSE 0 END), 0) AS rotting_2_to_3_days
                FROM organizations o
                LEFT JOIN deals d
                  ON d.organization_id = o.id
                 AND (d.is_deleted = 0 OR d.is_deleted IS NULL)
                 AND d.status = 'IN_PROGRESS'
                LEFT JOIN deal_stage_history h
                  ON h.deal_id = d.id
                 AND h.entered_at < ?
                 AND (h.exited_at IS NULL OR h.exited_at >= ?)
                LEFT JOIN stages s ON s.id = h.stage_id AND s.name = 'Qualified'
                WHERE s.name = 'Qualified' OR s.name IS NULL
                GROUP BY o.id, o.name
                ORDER BY rotting_2_to_3_days DESC, o.name ASC
                """;
        return jdbcTemplate.queryForList(sql, asOf, asOf, asOf, asOf);
    }

    private long sumLong(List<Map<String, Object>> rows, String key) {
        long sum = 0L;
        for (Map<String, Object> r : rows) {
            Object v = r.get(key);
            if (v instanceof Number n) {
                sum += n.longValue();
            } else if (v != null) {
                try {
                    sum += Long.parseLong(String.valueOf(v));
                } catch (Exception ignored) {
                }
            }
        }
        return sum;
    }

    private String wrapHtml(LocalDate reportDate, LocalDateTime start, LocalDateTime end, String inner) {
        String title = "Brideside CRM - Daily Ops Report";
        String preheader = "Daily ops summary for " + reportDate.format(PRETTY_DATE);
        return "<!doctype html>"
                + "<html><head><meta charset=\"utf-8\">"
                + "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
                + "<title>" + escape(title) + "</title>"
                + "</head>"
                + "<body style=\"margin:0;padding:0;background:#f6f7fb;font-family:ui-sans-serif,system-ui,-apple-system,Segoe UI,Roboto,Helvetica,Arial;\">"
                + "<div style=\"display:none;max-height:0;overflow:hidden;opacity:0;color:transparent;\">"
                + escape(preheader)
                + "</div>"
                + "<table role=\"presentation\" width=\"100%\" cellspacing=\"0\" cellpadding=\"0\" style=\"background:#f6f7fb;padding:24px 12px;\">"
                + "<tr><td align=\"center\">"
                + "<table role=\"presentation\" width=\"680\" cellspacing=\"0\" cellpadding=\"0\" style=\"max-width:680px;width:100%;\">"
                + header(reportDate, start, end)
                + "<tr><td style=\"padding:16px 0 0 0;\">" + inner + "</td></tr>"
                + footer()
                + "</table>"
                + "</td></tr></table>"
                + "</body></html>";
    }

    private String header(LocalDate reportDate, LocalDateTime start, LocalDateTime end) {
        String dateLabel = reportDate.format(PRETTY_DATE);
        return ""
                + "<tr><td style=\"background:linear-gradient(135deg,#0b1220,#111827);border-radius:18px;padding:22px 22px 18px 22px;\">"
                + "<div style=\"color:#ffffff;font-size:20px;font-weight:800;letter-spacing:-0.2px;line-height:1.2;\">Brideside CRM</div>"
                + "<div style=\"color:#d1d5db;font-size:13px;margin-top:6px;\">Daily Ops Report</div>"
                + "<div style=\"color:#9ca3af;font-size:12px;margin-top:10px;line-height:1.4;\">"
                + "<span style=\"display:inline-block;background:#111827;border:1px solid rgba(255,255,255,0.12);padding:6px 10px;border-radius:999px;\">"
                + "Report date: <b style=\"color:#ffffff;font-weight:700;\">" + escape(dateLabel) + "</b>"
                + "</span>"
                + "<span style=\"display:inline-block;margin-left:8px;background:#111827;border:1px solid rgba(255,255,255,0.12);padding:6px 10px;border-radius:999px;\">"
                + "Window: <b style=\"color:#ffffff;font-weight:700;\">" + escape(String.valueOf(start)) + "</b> → <b style=\"color:#ffffff;font-weight:700;\">" + escape(String.valueOf(end)) + "</b>"
                + "</span>"
                + "</div>"
                + "</td></tr>";
    }

    private String footer() {
        return "<tr><td style=\"padding:18px 2px 0 2px;color:#6b7280;font-size:12px;\">"
                + "This email was generated automatically by Brideside CRM."
                + "</td></tr>";
    }

    private String sectionKpis(long totalNewDeals, long totalBot, long totalManual, long won, long lost, long calls, long callMinutes) {
        return "<table role=\"presentation\" width=\"100%\" cellspacing=\"0\" cellpadding=\"0\" style=\"margin:0 0 16px 0;\">"
                + "<tr>"
                + kpiCard("New deals", fmt(totalNewDeals))
                + kpiCard("BOT deals", fmt(totalBot))
                + kpiCard("Manual (PreSales)", fmt(totalManual))
                + "</tr><tr>"
                + kpiCard("Won", fmt(won))
                + kpiCard("Lost", fmt(lost))
                + kpiCard("Calls / minutes", fmt(calls) + " / " + fmt(callMinutes))
                + "</tr></table>";
    }

    private String kpiCard(String label, String value) {
        return "<td style=\"padding:6px;\">"
                + "<table role=\"presentation\" width=\"100%\" cellspacing=\"0\" cellpadding=\"0\" style=\"background:#ffffff;border:1px solid #e5e7eb;border-radius:14px;\">"
                + "<tr><td style=\"padding:14px 14px 12px 14px;\">"
                + "<div style=\"color:#6b7280;font-size:12px;\">" + escape(label) + "</div>"
                + "<div style=\"color:#111827;font-size:18px;font-weight:700;margin-top:6px;\">" + escape(value) + "</div>"
                + "</td></tr></table>"
                + "</td>";
    }

    private String section(String title, String subtitle, String tableHtml) {
        return "<table role=\"presentation\" width=\"100%\" cellspacing=\"0\" cellpadding=\"0\" style=\"background:#ffffff;border:1px solid #e5e7eb;border-radius:16px;margin:0 0 14px 0;\">"
                + "<tr><td style=\"padding:16px 18px 8px 18px;\">"
                + "<div style=\"color:#111827;font-size:14px;font-weight:800;letter-spacing:-0.1px;\">" + escape(title) + "</div>"
                + "<div style=\"color:#6b7280;font-size:12px;margin-top:4px;\">" + escape(subtitle) + "</div>"
                + "</td></tr>"
                + "<tr><td style=\"padding:0 18px 16px 18px;\">" + tableHtml + "</td></tr>"
                + "</table>";
    }

    private String htmlTable(List<Map<String, Object>> rows, List<String> columns, List<String> metricColumns, boolean hideZeros, int maxRows) {
        if (rows == null || rows.isEmpty()) {
            return "<div style=\"color:#6b7280;font-size:13px;\">No data</div>";
        }

        int totalRows = rows.size();
        List<Map<String, Object>> filtered = hideZeros ? filterNonZero(rows, metricColumns) : rows;
        int nonZeroRows = filtered.size();
        List<Map<String, Object>> display = filtered.size() > maxRows ? filtered.subList(0, maxRows) : filtered;

        String meta = "<div style=\"color:#6b7280;font-size:12px;margin:0 0 10px 0;\">"
                + "Showing <b style=\"color:#111827;\">" + escape(String.valueOf(display.size())) + "</b>"
                + " of <b style=\"color:#111827;\">" + escape(String.valueOf(nonZeroRows)) + "</b>"
                + (hideZeros ? " non-zero" : "")
                + (totalRows != nonZeroRows ? " (total rows: " + escape(String.valueOf(totalRows)) + ")" : "")
                + "</div>";

        String thead = columns.stream()
                .map(c -> {
                    String align = metricColumns.contains(c) ? "right" : "left";
                    return "<th align=\"" + align + "\" style=\"padding:10px 10px;border-bottom:1px solid #e5e7eb;color:#6b7280;font-size:12px;font-weight:800;\">"
                            + escape(prettyHeader(c))
                            + "</th>";
                })
                .collect(Collectors.joining());

        StringBuilder tbody = new StringBuilder();
        for (int i = 0; i < display.size(); i++) {
            Map<String, Object> r = display.get(i);
            String bg = (i % 2 == 0) ? "#ffffff" : "#f9fafb";
            tbody.append("<tr style=\"background:").append(bg).append(";\">");
            for (String c : columns) {
                String align = metricColumns.contains(c) ? "right" : "left";
                tbody.append("<td align=\"").append(align).append("\" style=\"padding:10px 10px;border-bottom:1px solid #f3f4f6;color:#111827;font-size:13px;\">")
                        .append(escape(formatCell(r.get(c), metricColumns.contains(c))))
                        .append("</td>");
            }
            tbody.append("</tr>");
        }

        return meta
                + "<table role=\"presentation\" width=\"100%\" cellspacing=\"0\" cellpadding=\"0\" style=\"border-collapse:collapse;border:1px solid #eef2f7;border-radius:14px;overflow:hidden;\">"
                + "<thead><tr style=\"background:#f3f4f6;\">" + thead + "</tr></thead>"
                + "<tbody>" + tbody + "</tbody>"
                + "</table>";
    }

    private String prettyHeader(String key) {
        return key.replace('_', ' ').trim();
    }

    private List<Map<String, Object>> filterNonZero(List<Map<String, Object>> rows, List<String> metricColumns) {
        if (metricColumns == null || metricColumns.isEmpty()) return rows;
        return rows.stream().filter(r -> {
            for (String k : metricColumns) {
                Object v = r.get(k);
                if (v instanceof Number n) {
                    if (n.longValue() != 0L) return true;
                } else if (v != null) {
                    try {
                        if (Long.parseLong(String.valueOf(v)) != 0L) return true;
                    } catch (Exception ignored) {
                    }
                }
            }
            return false;
        }).collect(Collectors.toList());
    }

    private String formatCell(Object v, boolean numeric) {
        if (v == null) return "";
        if (!numeric) return String.valueOf(v);
        if (v instanceof Number n) return fmt(n.longValue());
        try {
            return fmt(Long.parseLong(String.valueOf(v)));
        } catch (Exception ignored) {
            return String.valueOf(v);
        }
    }

    private String fmt(long n) {
        return String.format(Locale.US, "%,d", n);
    }

    private String escape(String s) {
        if (s == null) return "";
        return s.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;")
                .replace("'", "&#39;");
    }

    private String formatOrgTable(List<Map<String, Object>> rows, String valueKey) {
        List<String> headers = List.of("organization_id", "organization_name", valueKey);
        return formatTable(headers, rows);
    }

    private String formatOrgMultiTable(List<Map<String, Object>> rows, List<String> valueKeys) {
        List<String> headers = new ArrayList<>();
        headers.add("organization_id");
        headers.add("organization_name");
        headers.addAll(valueKeys);
        return formatTable(headers, rows);
    }

    private String formatUserCallsTable(List<Map<String, Object>> rows) {
        return formatTable(List.of("user_id", "user_name", "role_name", "calls_done", "total_call_minutes"), rows);
    }

    private String formatCityTable(List<Map<String, Object>> rows) {
        return formatTable(List.of("city", "deals_created"), rows);
    }

    private String formatTable(List<String> headers, List<Map<String, Object>> rows) {
        // Compute widths
        List<Integer> widths = new ArrayList<>(headers.size());
        for (String h : headers) {
            widths.add(h.length());
        }
        for (Map<String, Object> r : rows) {
            for (int i = 0; i < headers.size(); i++) {
                String key = headers.get(i);
                String val = toCell(r.get(key));
                widths.set(i, Math.max(widths.get(i), val.length()));
            }
        }

        StringJoiner out = new StringJoiner("\n");
        out.add(renderRow(headers, widths));
        out.add(renderSeparator(widths));
        for (Map<String, Object> r : rows) {
            List<String> cells = new ArrayList<>(headers.size());
            for (String key : headers) {
                cells.add(toCell(r.get(key)));
            }
            out.add(renderRow(cells, widths));
        }
        return out.toString();
    }

    private String renderRow(List<String> cells, List<Integer> widths) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < cells.size(); i++) {
            if (i > 0) sb.append(" | ");
            sb.append(padRight(cells.get(i), widths.get(i)));
        }
        return sb.toString();
    }

    private String renderSeparator(List<Integer> widths) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < widths.size(); i++) {
            if (i > 0) sb.append("-+-");
            sb.append("-".repeat(widths.get(i)));
        }
        return sb.toString();
    }

    private String toCell(Object v) {
        if (v == null) return "";
        return String.valueOf(v);
    }

    private String padRight(String s, int width) {
        if (s == null) s = "";
        if (s.length() >= width) return s;
        return s + " ".repeat(width - s.length());
    }
}

