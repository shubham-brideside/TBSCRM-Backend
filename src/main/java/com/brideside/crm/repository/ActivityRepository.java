package com.brideside.crm.repository;

import com.brideside.crm.entity.Activity;
import com.brideside.crm.entity.Deal;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ActivityRepository extends JpaRepository<Activity, Long> {
    List<Activity> findByDealOrderByDateTimeDesc(Deal deal);
}



