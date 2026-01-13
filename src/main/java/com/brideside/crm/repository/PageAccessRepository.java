package com.brideside.crm.repository;

import com.brideside.crm.entity.PageAccess;
import com.brideside.crm.entity.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface PageAccessRepository extends JpaRepository<PageAccess, Long> {
    List<PageAccess> findByUser(User user);
    List<PageAccess> findByUserId(Long userId);
    Optional<PageAccess> findByUserAndPageName(User user, String pageName);
    Optional<PageAccess> findByUserIdAndPageName(Long userId, String pageName);
    void deleteByUser(User user);
    void deleteByUserId(Long userId);
}

