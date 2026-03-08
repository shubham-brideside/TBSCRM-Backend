package com.brideside.crm.repository;

import com.brideside.crm.entity.ClientData;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface ClientDataRepository extends JpaRepository<ClientData, Long> {

    Optional<ClientData> findByOrganization_Id(Long organizationId);

    boolean existsByOrganization_Id(Long organizationId);
}
