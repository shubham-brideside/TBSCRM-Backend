package com.brideside.crm.service;

import java.util.Set;

/**
 * Row-level scope for deal visibility. Admins are unrestricted; other roles are limited
 * to specific pipelines (and optionally orgs when a deal has no pipeline).
 * <p>
 * Category managers with {@link #categoryManagerCategoryScope()} set (managed vertical) see only
 * pipelines/deals for that product category — not org-owner pipelines from other verticals.
 * Without a managed category, they use {@link #matchDealsByOrganizationId()} over organizations they
 * own (same tree as organization list).
 */
public record DealAccessScope(
        /** True when the user may see all pipelines/deals (e.g. admin). */
        boolean fullAccess,
        Set<Long> allowedPipelineIds,
        Set<Long> nullablePipelineOrganizationIds,
        /**
         * When true (category managers without {@link #categoryManagerCategoryScope()}), any
         * non-deleted deal with {@code organization_id} in {@link #nullablePipelineOrganizationIds()}
         * matches, regardless of {@code pipeline_id}.
         */
        boolean matchDealsByOrganizationId,
        /**
         * When non-null, category manager visibility is driven by this category (pipelines with
         * matching {@code pipelines.category}, deals with matching deal/org category).
         */
        CategoryManagerCategoryScope categoryManagerCategoryScope
) {
    /**
     * Resolved from {@code users.user_managed_category_id} + {@code categories.name}.
     */
    public record CategoryManagerCategoryScope(Long categoryId, String categoryName) {}

    /** Admin / no row-level pipeline filter. */
    public static DealAccessScope unrestricted() {
        return new DealAccessScope(true, Set.of(), Set.of(), false, null);
    }

    public static DealAccessScope noAccess() {
        return new DealAccessScope(false, Set.of(), Set.of(), false, null);
    }
}
