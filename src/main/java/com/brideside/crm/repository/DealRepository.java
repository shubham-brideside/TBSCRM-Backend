package com.brideside.crm.repository;

import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.Pipeline;
import com.brideside.crm.entity.Stage;
import com.brideside.crm.entity.DealStatus;
import com.brideside.crm.entity.Person;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Category;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface DealRepository extends JpaRepository<Deal, Long> {
    List<Deal> findByPipeline(Pipeline pipeline);
    List<Deal> findByStage(Stage stage);
    List<Deal> findByStatus(DealStatus status);
    List<Deal> findByPerson(Person person);
    List<Deal> findByOrganization(Organization organization);
    List<Deal> findByDealCategory(Category category);
}



