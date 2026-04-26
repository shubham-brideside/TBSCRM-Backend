package com.brideside.crm.constants;

import com.brideside.crm.entity.Role;

/**
 * TBS onboarding roles and helpers. Granted authorities use {@code ROLE_} + {@link Role.RoleName#name()}.
 */
public final class TbsRoles {

    private TbsRoles() {
    }

    public static boolean isTbs(Role.RoleName roleName) {
        if (roleName == null) {
            return false;
        }
        return roleName == Role.RoleName.TBS_PRESALES
                || roleName == Role.RoleName.TBS_REL_MANAGER
                || roleName == Role.RoleName.TBS_SVC_MANAGER;
    }
}
