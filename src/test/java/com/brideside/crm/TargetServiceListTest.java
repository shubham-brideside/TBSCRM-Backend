package com.brideside.crm;

import com.brideside.crm.dto.TargetDtos;
import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.User;
import com.brideside.crm.repository.UserRepository;
import com.brideside.crm.service.TargetService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.List;

@SpringBootTest
class TargetServiceListTest {

    @Autowired
    private TargetService targetService;

    @Autowired
    private UserRepository userRepository;

    @BeforeEach
    void setUpSecurityContext() {
        List<User> admins = userRepository.findByRole_NameAndActiveTrue(Role.RoleName.ADMIN);
        User admin = admins.stream().findFirst()
                .orElseThrow(() -> new IllegalStateException("No admin user available for test"));
        SecurityContextHolder.getContext().setAuthentication(
                new UsernamePasswordAuthenticationToken(
                        admin.getEmail(),
                        "N/A",
                        List.of(new SimpleGrantedAuthority("ROLE_" + admin.getRole().getName().name()))
                )
        );
    }

    @Test
    void loadTargetsFor2025() {
        TargetDtos.TargetListFilter filter = new TargetDtos.TargetListFilter();
        filter.year = 2025;
        targetService.list(filter);
    }
}

