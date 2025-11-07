package com.brideside.crm.service.impl;

import com.brideside.crm.dto.SimpleDtos;
import com.brideside.crm.entity.Source;
import com.brideside.crm.repository.SourceRepository;
import com.brideside.crm.service.SourceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class SourceServiceImpl implements SourceService {

    @Autowired private SourceRepository sourceRepository;

    @Override
    public Source create(SimpleDtos.SourceCreate req) {
        Source s = new Source();
        s.setType(req.type);
        s.setCommissionPercentage(req.commissionPercentage);
        return sourceRepository.save(s);
    }

    @Override
    public List<Source> list() { return sourceRepository.findAll(); }
}



