package com.brideside.crm.repository;

import com.brideside.crm.entity.Role;
import com.brideside.crm.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
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
    List<User> findByActiveTrue();

    /**
     * Writes {@code user_managed_category_id} with SQL so the value is always persisted regardless of
     * JPA association/caching quirks. Caller must run inside a transaction.
     */
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query(value = "UPDATE users SET user_managed_category_id = :categoryId WHERE id = :userId", nativeQuery = true)
    int updateManagedCategoryId(@Param("userId") Long userId, @Param("categoryId") Long categoryId);
}

