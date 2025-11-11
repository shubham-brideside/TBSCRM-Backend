package com.brideside.crm.repository;

import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {
    Optional<User> findByEmail(String email);
    Boolean existsByEmail(String email);
    List<User> findByManagerId(Long managerId);
    List<User> findByRole_NameInAndActiveTrue(Iterable<Role.RoleName> roleNames);
    List<User> findByRole_NameAndActiveTrue(Role.RoleName roleName);
}

