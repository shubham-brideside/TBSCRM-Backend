package com.brideside.crm.dto;

import com.brideside.crm.entity.Team;
import com.brideside.crm.entity.User;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import java.time.Instant;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public final class TeamDtos {

    private TeamDtos() {
    }

    public static class TeamRequest {
        @NotBlank(message = "Team name is required")
        @Size(max = 255, message = "Team name cannot exceed 255 characters")
        private String name;

        private Long managerId;

        private Boolean clearManager;

        private List<Long> memberIds;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public Long getManagerId() {
            return managerId;
        }

        public void setManagerId(Long managerId) {
            this.managerId = managerId;
        }

        public Boolean getClearManager() {
            return clearManager;
        }

        public void setClearManager(Boolean clearManager) {
            this.clearManager = clearManager;
        }

        public List<Long> getMemberIds() {
            return memberIds;
        }

        public void setMemberIds(List<Long> memberIds) {
            this.memberIds = memberIds;
        }
    }

    public static class TeamResponse {
        private Long id;
        private String name;
        private UserSummary manager;
        private List<UserSummary> members = List.of();
        private Instant createdAt;
        private Instant updatedAt;

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

        public UserSummary getManager() {
            return manager;
        }

        public void setManager(UserSummary manager) {
            this.manager = manager;
        }

        public List<UserSummary> getMembers() {
            return members;
        }

        public void setMembers(List<UserSummary> members) {
            this.members = members;
        }

        public Instant getCreatedAt() {
            return createdAt;
        }

        public void setCreatedAt(Instant createdAt) {
            this.createdAt = createdAt;
        }

        public Instant getUpdatedAt() {
            return updatedAt;
        }

        public void setUpdatedAt(Instant updatedAt) {
            this.updatedAt = updatedAt;
        }
    }

    public static class UserSummary {
        private Long id;
        private String firstName;
        private String lastName;
        private String email;
        private String role;

        public Long getId() {
            return id;
        }

        public void setId(Long id) {
            this.id = id;
        }

        public String getFirstName() {
            return firstName;
        }

        public void setFirstName(String firstName) {
            this.firstName = firstName;
        }

        public String getLastName() {
            return lastName;
        }

        public void setLastName(String lastName) {
            this.lastName = lastName;
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

        public String getDisplayName() {
            StringBuilder builder = new StringBuilder();
            if (firstName != null) builder.append(firstName);
            if (lastName != null && !lastName.isBlank()) {
                if (builder.length() > 0) builder.append(' ');
                builder.append(lastName);
            }
            return builder.length() > 0 ? builder.toString() : email;
        }
    }

    public static TeamResponse toResponse(Team team) {
        TeamResponse response = new TeamResponse();
        response.setId(team.getId());
        response.setName(team.getName());
        response.setManager(team.getManager() != null ? toSummary(team.getManager()) : null);
        response.setMembers(toSummaries(team.getMembers()));
        response.setCreatedAt(team.getCreatedAt());
        response.setUpdatedAt(team.getUpdatedAt());
        return response;
    }

    public static List<TeamResponse> toResponses(List<Team> teams) {
        return teams.stream()
                .map(TeamDtos::toResponse)
                .collect(Collectors.toList());
    }

    public static UserSummary toSummary(User user) {
        if (user == null) return null;
        UserSummary summary = new UserSummary();
        summary.setId(user.getId());
        summary.setFirstName(user.getFirstName());
        summary.setLastName(user.getLastName());
        summary.setEmail(user.getEmail());
        summary.setRole(user.getRole() != null ? user.getRole().getName().name() : null);
        return summary;
    }

    public static List<UserSummary> toSummaries(Set<User> users) {
        if (users == null || users.isEmpty()) return List.of();
        return users.stream()
                .map(TeamDtos::toSummary)
                .collect(Collectors.toList());
    }
}


