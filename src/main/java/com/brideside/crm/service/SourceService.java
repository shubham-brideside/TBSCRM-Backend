package com.brideside.crm.service;

import com.brideside.crm.dto.SimpleDtos;
import com.brideside.crm.entity.Source;

import java.util.List;

public interface SourceService {
    Source create(SimpleDtos.SourceCreate req);
    List<Source> list();
}



