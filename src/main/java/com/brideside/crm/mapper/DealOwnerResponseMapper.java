package com.brideside.crm.mapper;

import com.brideside.crm.dto.DealResponse;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.User;
import com.brideside.crm.repository.UserRepository;

/**
 * Resolves {@link DealResponse#ownerId} / {@link DealResponse#ownerDisplayName} for API consumers
 * (including PRESALES): denormalized {@link Deal#getOwner()}, then {@code deals.owner_id},
 * then {@code person.owner}, then {@code organization.owner}.
 */
public final class DealOwnerResponseMapper {

    private DealOwnerResponseMapper() {
    }

    public static void populateOwner(DealResponse r, Deal d, UserRepository userRepository) {
        User ownerUser = null;
        if (d.getOwner() != null) {
            ownerUser = d.getOwner();
        } else if (d.getOwnerId() != null && userRepository != null) {
            ownerUser = userRepository.findById(d.getOwnerId()).orElse(null);
        }
        if (ownerUser != null) {
            r.ownerId = ownerUser.getId();
            r.ownerDisplayName = ownerUser.getDisplayName();
            return;
        }
        if (d.getPerson() != null && d.getPerson().getOwner() != null) {
            User po = d.getPerson().getOwner();
            r.ownerId = po.getId();
            r.ownerDisplayName = po.getDisplayName();
            return;
        }
        if (d.getOrganization() != null && d.getOrganization().getOwner() != null) {
            User oo = d.getOrganization().getOwner();
            r.ownerId = oo.getId();
            r.ownerDisplayName = oo.getDisplayName();
            return;
        }
        r.ownerId = d.getOwnerId();
        r.ownerDisplayName = null;
    }
}
