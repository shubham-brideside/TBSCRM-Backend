package com.brideside.crm.repository;

import com.brideside.crm.entity.Deal;
import com.brideside.crm.entity.Pipeline;
import com.brideside.crm.entity.Stage;
import com.brideside.crm.entity.DealStatus;
import com.brideside.crm.entity.Person;
import com.brideside.crm.entity.Organization;
import com.brideside.crm.entity.Category;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;

public interface DealRepository extends JpaRepository<Deal, Long>, JpaSpecificationExecutor<Deal> {
    List<Deal> findByPipeline(Pipeline pipeline);
    List<Deal> findByStage(Stage stage);
    List<Deal> findByStatus(DealStatus status);
    List<Deal> findByPerson(Person person);
    List<Deal> findByOrganization(Organization organization);
    List<Deal> findByDealCategory(Category category);

    @Query("select distinct d from Deal d " +
            "left join fetch d.person p " +
            "left join fetch p.owner owner " +
            "left join fetch d.organization org " +
            "left join fetch d.dealCategory cat " +
            "left join fetch d.source src " +
            "where d.status = com.brideside.crm.entity.DealStatus.WON " +
            "and ( (d.updatedAt is not null and d.updatedAt >= :start and d.updatedAt < :end) " +
            "   or (d.updatedAt is null and d.createdAt >= :start and d.createdAt < :end) )")
    List<Deal> findWonDealsUpdatedBetween(
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end);
    List<Deal> findByReferencedDeal(Deal referencedDeal);
    boolean existsByReferencedDealAndPipeline(Deal referencedDeal, Pipeline pipeline);
    
    // Methods that exclude deleted deals
    List<Deal> findByIsDeletedFalse();
    
    @Query("SELECT d FROM Deal d LEFT JOIN FETCH d.person LEFT JOIN FETCH d.organization WHERE d.isDeleted = false OR d.isDeleted IS NULL")
    List<Deal> findByIsDeletedFalseWithPersonAndOrganization();
    
    List<Deal> findByPipelineAndIsDeletedFalse(Pipeline pipeline);
    List<Deal> findByStageAndIsDeletedFalse(Stage stage);
    List<Deal> findByStatusAndIsDeletedFalse(DealStatus status);
    List<Deal> findByPersonAndIsDeletedFalse(Person person);
    List<Deal> findByOrganizationAndIsDeletedFalse(Organization organization);
    List<Deal> findByDealCategoryAndIsDeletedFalse(Category category);
    List<Deal> findByReferencedDealAndIsDeletedFalse(Deal referencedDeal);
}



