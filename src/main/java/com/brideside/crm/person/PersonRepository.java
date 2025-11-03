package com.brideside.crm.person;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import java.util.List;

public interface PersonRepository extends JpaRepository<Person, Long>, JpaSpecificationExecutor<Person> {
    @Query("select distinct p.category from Person p where p.category is not null order by p.category")
    List<String> findDistinctCategories();

    @Query("select distinct p.organization from Person p where p.organization is not null order by p.organization")
    List<String> findDistinctOrganizations();

    @Query("select distinct p.manager from Person p where p.manager is not null order by p.manager")
    List<String> findDistinctManagers();

    @Query("select distinct p.manager from Person p where p.category = :category and p.manager is not null order by p.manager")
    List<String> findManagersByCategory(@Param("category") String category);

    @Query("select distinct p.manager from Person p where p.organization = :organization and p.manager is not null order by p.manager")
    List<String> findManagersByOrganization(@Param("organization") String organization);

    @Query("select distinct p.venue from Person p where p.venue is not null order by p.venue")
    List<String> findDistinctVenues();
}


