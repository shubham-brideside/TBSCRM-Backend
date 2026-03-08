package com.brideside.crm.service;

import com.brideside.crm.dto.ClientDataDtos;
import org.springframework.web.multipart.MultipartFile;

public interface ClientDataService {

    ClientDataDtos.ClientDataResponse getByOrganization(Long organizationId);

    ClientDataDtos.ClientDataResponse uploadQuoteFormat(Long organizationId, MultipartFile file);

    ClientDataDtos.ClientDataResponse uploadClientContractFormat(Long organizationId, MultipartFile file);

    ClientDataDtos.ClientDataResponse removeQuoteFormat(Long organizationId);

    ClientDataDtos.ClientDataResponse removeClientContractFormat(Long organizationId);

    void delete(Long organizationId);
}
