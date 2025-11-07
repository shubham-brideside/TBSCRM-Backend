package com.brideside.crm.service;

import com.brideside.crm.dto.ActivityDtos;
import com.brideside.crm.entity.Activity;

import java.util.List;

public interface ActivityService {
    Activity create(ActivityDtos.CreateRequest request);
    List<Activity> listByDeal(Long dealId);
}



