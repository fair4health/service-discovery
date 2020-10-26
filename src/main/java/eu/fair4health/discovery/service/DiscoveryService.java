/*
 * Copyright (C) 2019  Atos Spain SA. All rights reserved.
 * 
 * This file is part of the Spring Boot template project.
 * 
 * This is free software: you can redistribute it and/or modify it under the 
 * terms of the Apache License, Version 2.0 (the License);
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * The software is provided "AS IS", without any warranty of any kind, express or implied,
 * including but not limited to the warranties of merchantability, fitness for a particular
 * purpose and noninfringement, in no event shall the authors or copyright holders be 
 * liable for any claim, damages or other liability, whether in action of contract, tort or
 * otherwise, arising from, out of or in connection with the software or the use or other
 * dealings in the software.
 * 
 * See README file for the full disclaimer information and LICENSE file for full license 
 * information in the project root.
 * 
 * Calculator Spring boot service
 */
package eu.fair4health.discovery.service;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.agent.model.NewService;

import eu.fair4health.discovery.model.AgentService;

@Component
public class DiscoveryService implements Service {

    public static final Logger log = LoggerFactory.getLogger(DiscoveryService.class);
    
    // Consul properties
    @Value("${spring.cloud.consul.host}")
    private String consulHost;

    @Value("${spring.cloud.consul.port}")
    private Integer consulPort;
    
    // Consul Service properties
    @Value("${spring.cloud.consul.discovery.health-check-interval}")
    private String healthCheckInterval;
    
    @Value("${spring.cloud.consul.discovery.health-check-timeout}")
    private String healthCheckTimeout;
    
    private static final String HTTPS = "HTTPS";
    private static final String SECURE = "secure=";
    
    // REST Template
    RestTemplate restTemplate = new RestTemplate();

    // Inject the Discovery Client
    @Autowired
    private DiscoveryClient discoveryClient;

    @Override
    public void register(AgentService service) throws MalformedURLException {
    	
    	// Check if agent exist in Consul
    	List<ServiceInstance> serviceList = discover(service.getServiceId()); 
    	if (serviceList.size() > 0) {
			return;
		}
    	
        ConsulClient client = new ConsulClient(consulHost, consulPort);
        
        URL agentUrl = new URL(service.getUrl());

        NewService newService = new NewService();
        newService.setId(service.getServiceId());
        newService.setName(service.getServiceId());
        
        Integer port = agentUrl.getPort();
        if (port == -1)
            port = agentUrl.getDefaultPort();
        newService.setPort(port);
        
        if (service.getMetadata() != null)
            newService.setMeta(service.getMetadata());
        newService.setAddress(agentUrl.getHost());
        
        List<String> tags = service.getTags();
        if (service.getTags() == null)
            tags = new ArrayList<String>();
        if (HTTPS.equalsIgnoreCase(agentUrl.getProtocol()))
            tags.add(SECURE + Boolean.TRUE.toString());
        else
            tags.add(SECURE + Boolean.FALSE.toString());
        
        newService.setTags(tags);

        NewService.Check serviceCheck = new NewService.Check();
        serviceCheck.setHttp(service.getHealthCheck());
        serviceCheck.setInterval(healthCheckInterval);
        serviceCheck.setTimeout(healthCheckTimeout);
        newService.setCheck(serviceCheck);
        
        client.agentServiceRegister(newService);

        return;
    }

    @Override
    public List<ServiceInstance> discover(String serviceId) {
        if (serviceId.trim().isEmpty() == true)
            return new ArrayList<ServiceInstance>();

        List<ServiceInstance> instances = discoveryClient.getInstances(serviceId);
        return instances;
    }

    @Override
    public List<ServiceInstance> discoverAll() {

        List<ServiceInstance> instances = new ArrayList<ServiceInstance>();

        List<String> services = discoveryClient.getServices();
        for (String s : services) {
            List<ServiceInstance> instancesByService = discoveryClient.getInstances(s);
            for (ServiceInstance SI : instancesByService)
                instances.add(SI);
        }
        
        return instances;
    }

    @Override
    public void deregister(String serviceId) {
        ConsulClient client = new ConsulClient(consulHost, consulPort);
        client.agentServiceDeregister(serviceId);
    }
    
    public void setRestTemplate(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }
}