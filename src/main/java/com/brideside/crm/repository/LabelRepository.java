package com.brideside.crm.repository;

import com.brideside.crm.entity.Label;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface LabelRepository extends JpaRepository<Label, Long> {
    Optional<Label> findByName(String name);
    List<Label> findByNameContainingIgnoreCase(String name);
    boolean existsByName(String name);
}

