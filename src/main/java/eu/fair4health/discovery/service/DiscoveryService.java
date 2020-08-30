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

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import com.ecwid.consul.v1.agent.model.NewService;

@Component
public class DiscoveryService implements Service {

    public static final Logger log = LoggerFactory.getLogger(DiscoveryService.class);

    // Constants
    RestTemplate restTemplate = new RestTemplate();

    // Inject the Discovery Client
    @Autowired
    private DiscoveryClient discoveryClient;

    @Override
    public void register(NewService service) {
        return;
    }

    @Override
    public List<ServiceInstance> discover(String name) {
        if (name.trim().isEmpty() == true)
            return new ArrayList<ServiceInstance>();

        List<ServiceInstance> instances = discoveryClient.getInstances(name);
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

    public void setRestTemplate(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }
}