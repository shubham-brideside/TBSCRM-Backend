package com.brideside.crm.dto;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Patch for {@code users.user_managed_category_id} (category managers; admin or self).
 * Use {@code null} to clear the assignment.
 */
@Schema(description = "Set or clear the product category a category manager oversees")
public class PatchManagedCategoryRequest {

    @Schema(description = "categories.id for this vertical (e.g. Photography), or null to clear")
    private Long managedCategoryId;

    public Long getManagedCategoryId() {
        return managedCategoryId;
    }

    public void setManagedCategoryId(Long managedCategoryId) {
        this.managedCategoryId = managedCategoryId;
    }
}
