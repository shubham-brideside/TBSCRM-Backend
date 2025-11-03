package com.brideside.crm.repository;

import com.brideside.crm.entity.InvitationToken;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface InvitationTokenRepository extends JpaRepository<InvitationToken, Long> {
    Optional<InvitationToken> findByToken(String token);
    Optional<InvitationToken> findByTokenAndUsedFalse(String token);
    
    @Modifying
    @Query("DELETE FROM InvitationToken it WHERE it.user.id = :userId")
    void deleteByUserId(@Param("userId") Long userId);
}

