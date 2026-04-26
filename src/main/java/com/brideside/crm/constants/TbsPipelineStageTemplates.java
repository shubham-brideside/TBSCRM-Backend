package com.brideside.crm.constants;

import com.brideside.crm.entity.Role;
import com.brideside.crm.exception.BadRequestException;

import java.util.List;
import java.util.Objects;

/**
 * Default stage names for pipelines created during TBS user onboarding.
 * Organization IDs must match {@code app.tbs.user-onboarding} in {@code application.yml}.
 */
public final class TbsPipelineStageTemplates {

    private TbsPipelineStageTemplates() {
    }

    public static List<String> stagesFor(
            Role.RoleName tbsRole, long organizationId, TbsUserOnboardingProperties onboarding) {
        Objects.requireNonNull(tbsRole, "tbsRole");
        Objects.requireNonNull(onboarding, "onboarding");
        return switch (tbsRole) {
            case TBS_PRESALES -> List.of(
                    "Lead In",
                    "Number Captured",
                    "Call Attempted",
                    "Contact Reattempted",
                    "Qualified");
            case TBS_REL_MANAGER -> List.of(
                    "Lead Received",
                    "Evaluating",
                    "Meeting Scheduled",
                    "Meeting Done",
                    "Proposal Presented",
                    "Negotiation",
                    "Committed");
            case TBS_SVC_MANAGER -> stagesForServiceManager(organizationId, onboarding);
            default -> throw new BadRequestException("Not a TBS role: " + tbsRole);
        };
    }

    private static List<String> stagesForServiceManager(long organizationId, TbsUserOnboardingProperties onboarding) {
        if (organizationId == onboarding.getOrgTbsPlanningId()) {
            return List.of(
                    "Onboarded",
                    "Wedding Diary Shared",
                    "Non-venue Suggestions",
                    "Vendor Suggestions",
                    "Deposit Received");
        }
        if (organizationId == onboarding.getOrgRevaahId()) {
            return List.of(
                    "Onboarded",
                    "Theme Discussed",
                    "Customized Moodboard Shared",
                    "Deposit Received");
        }
        if (organizationId == onboarding.getOrgTbsVenueId()) {
            return List.of(
                    "Onboarded",
                    "Venue Proposed",
                    "Venue Shortlisted",
                    "Site Visit Scheduled",
                    "Site Visit Done",
                    "Re-visit Scheduled",
                    "Re-visit Done",
                    "Deposit Received");
        }
        throw new BadRequestException(
                "TBS Service Manager stages are not defined for organization id " + organizationId);
    }
}
