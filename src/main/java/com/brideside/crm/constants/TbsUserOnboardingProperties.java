package com.brideside.crm.constants;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Set;

/**
 * Configurable organization IDs for TBS user onboarding (defaults match product spec).
 */
@Component
public class TbsUserOnboardingProperties {

    public static final long DEFAULT_ORG_TBS_TEST_ID = 117L;
    public static final long DEFAULT_ORG_REVAAH_ID = 64L;
    public static final long DEFAULT_ORG_TBS_PLANNING_ID = 118L;
    public static final long DEFAULT_ORG_TBS_VENUE_ID = 119L;

    private final long orgTbsTestId;
    private final long orgRevaahId;
    private final long orgTbsPlanningId;
    private final long orgTbsVenueId;

    public TbsUserOnboardingProperties(
            @Value("${app.tbs.user-onboarding.organization-tbs-test-id:" + DEFAULT_ORG_TBS_TEST_ID + "}") long orgTbsTestId,
            @Value("${app.tbs.user-onboarding.organization-revaah-id:" + DEFAULT_ORG_REVAAH_ID + "}") long orgRevaahId,
            @Value("${app.tbs.user-onboarding.organization-tbs-planning-id:" + DEFAULT_ORG_TBS_PLANNING_ID + "}") long orgTbsPlanningId,
            @Value("${app.tbs.user-onboarding.organization-tbs-venue-id:" + DEFAULT_ORG_TBS_VENUE_ID + "}") long orgTbsVenueId) {
        this.orgTbsTestId = orgTbsTestId;
        this.orgRevaahId = orgRevaahId;
        this.orgTbsPlanningId = orgTbsPlanningId;
        this.orgTbsVenueId = orgTbsVenueId;
    }

    public long getOrgTbsTestId() {
        return orgTbsTestId;
    }

    public Set<Long> allowedServiceManagerOrganizationIds() {
        return Set.of(orgRevaahId, orgTbsPlanningId, orgTbsVenueId);
    }

    public long getOrgRevaahId() {
        return orgRevaahId;
    }

    public long getOrgTbsPlanningId() {
        return orgTbsPlanningId;
    }

    public long getOrgTbsVenueId() {
        return orgTbsVenueId;
    }
}
