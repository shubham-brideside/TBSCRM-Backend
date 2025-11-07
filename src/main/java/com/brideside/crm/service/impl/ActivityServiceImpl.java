package com.brideside.crm.service.impl;

import com.brideside.crm.dto.ActivityDtos;
import com.brideside.crm.entity.Activity;
import com.brideside.crm.entity.Deal;
import com.brideside.crm.exception.ResourceNotFoundException;
import com.brideside.crm.repository.ActivityRepository;
import com.brideside.crm.repository.DealRepository;
import com.brideside.crm.service.ActivityService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class ActivityServiceImpl implements ActivityService {

    @Autowired private ActivityRepository activityRepository;
    @Autowired private DealRepository dealRepository;

    @Override
    public Activity create(ActivityDtos.CreateRequest request) {
        Deal deal = dealRepository.findById(request.dealId)
                .orElseThrow(() -> new ResourceNotFoundException("Deal not found"));
        Activity a = new Activity();
        a.setDeal(deal);
        a.setType(request.type);
        a.setDateTime(request.dateTime);
        a.setStatus(request.status);
        a.setDurationMinutes(request.durationMinutes);
        return activityRepository.save(a);
    }

    @Override
    public List<Activity> listByDeal(Long dealId) {
        Deal deal = dealRepository.findById(dealId)
                .orElseThrow(() -> new ResourceNotFoundException("Deal not found"));
        return activityRepository.findByDealOrderByDateTimeDesc(deal);
    }
}



