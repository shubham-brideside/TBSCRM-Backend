package com.brideside.crm.dto;

import com.brideside.crm.entity.Activity;
import com.brideside.crm.service.ActivityScopeService;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class ActivityDtos {

    private ActivityDtos() {}

    public static class CreateRequest {
        public Long dealId;
        public String type;
        public LocalDateTime dateTime;
        public String status;
        public Long durationMinutes;
    }

    public static class CategoryOption {
        private String code;
        private String label;

        public CategoryOption() {}

        public CategoryOption(String code, String label) {
            this.code = code;
            this.label = label;
        }

        public String getCode() {
            return code;
        }

        public void setCode(String code) {
            this.code = code;
        }

        public String getLabel() {
            return label;
        }

        public void setLabel(String label) {
            this.label = label;
        }
    }

    public static class FilterOptions {
        private boolean restricted;
        private List<OrganizationFilter> organizations;
        private List<UserFilter> users;

        public boolean isRestricted() {
            return restricted;
        }

        public void setRestricted(boolean restricted) {
            this.restricted = restricted;
        }

        public List<OrganizationFilter> getOrganizations() {
            return organizations;
        }

        public void setOrganizations(List<OrganizationFilter> organizations) {
            this.organizations = organizations;
        }

        public List<UserFilter> getUsers() {
            return users;
        }

        public void setUsers(List<UserFilter> users) {
            this.users = users;
        }
    }

    public static class OrganizationFilter {
        private Long id;
        private String name;
        private String category;

        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getCategory() {
            return category;
        }

        public void setCategory(String category) {
            this.category = category;
        }
    }

    public static class UserFilter {
        private Long id;
        private String displayName;
        private String email;
        private String role;

        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public String getDisplayName() {
            return displayName;
        }

        public void setDisplayName(String displayName) {
            this.displayName = displayName;
        }

        public String getEmail() {
            return email;
        }

        public void setEmail(String email) {
            this.email = email;
        }

        public String getRole() {
            return role;
        }

        public void setRole(String role) {
            this.role = role;
        }
    }

    /**
     * Summary DTO for activities dashboard cards.
     * All counts are computed with the same role-based scope and filters as the main list endpoint.
     */
    public static class Summary {
        private long total;
        private long pending;
        private long completed;
        private long assignCall;
        private long meetingScheduled;

        public long getTotal() {
            return total;
        }

        public void setTotal(long total) {
            this.total = total;
        }

        public long getPending() {
            return pending;
        }

        public void setPending(long pending) {
            this.pending = pending;
        }

        public long getCompleted() {
            return completed;
        }

        public void setCompleted(long completed) {
            this.completed = completed;
        }

        public long getAssignCall() {
            return assignCall;
        }

        public void setAssignCall(long assignCall) {
            this.assignCall = assignCall;
        }

        public long getMeetingScheduled() {
            return meetingScheduled;
        }

        public void setMeetingScheduled(long meetingScheduled) {
            this.meetingScheduled = meetingScheduled;
        }
    }

    public static List<CategoryOption> allCategoryOptions() {
        return Arrays.stream(Activity.ActivityCategory.values())
                .map(cat -> new CategoryOption(cat.name(), prettify(cat.name())))
                .collect(Collectors.toList());
    }

    public static FilterOptions buildFilterOptions(ActivityScopeService.FilterContext ctx) {
        FilterOptions opts = new FilterOptions();
        opts.setRestricted(ctx.restricted());

        List<OrganizationFilter> orgs = ctx.organizations().stream().map(org -> {
            OrganizationFilter of = new OrganizationFilter();
            of.setId(org.getId());
            of.setName(org.getName());
            of.setCategory(org.getCategory() != null ? org.getCategory().getDbValue() : null);
            return of;
        }).collect(Collectors.toList());

        List<UserFilter> users = ctx.users().stream().map(user -> {
            UserFilter uf = new UserFilter();
            uf.setId(user.getId());
            uf.setEmail(user.getEmail());
            uf.setDisplayName(user.getFirstName() + " " + user.getLastName());
            uf.setRole(user.getRole() != null ? user.getRole().getName().name() : null);
            return uf;
        }).collect(Collectors.toList());

        opts.setOrganizations(orgs);
        opts.setUsers(users);
        return opts;
    }

    private static String prettify(String code) {
        String lower = code.toLowerCase().replace('_', ' ');
        return Arrays.stream(lower.split(" "))
                .map(word -> word.isEmpty() ? word : Character.toUpperCase(word.charAt(0)) + word.substring(1))
                .collect(Collectors.joining(" "));
    }
}

