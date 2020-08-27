/*
 * Copyright (C) 2020  Atos Spain SA. All rights reserved.
 * 
 * This file is part of the FAIR4Health Service Discovery.
 * 
 * AuthController.java is free software: you can redistribute it and/or modify it under the 
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
 * Spring boot controller for FAIR4Health Service Discovery
 */

package eu.fair4health.discovery.controller;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.ecwid.consul.v1.agent.model.NewService;

import eu.fair4health.discovery.service.Service;
import io.swagger.annotations.ApiOperation;

@RestController
@EnableAutoConfiguration
public class DiscoveryController {

    private static final Logger log = LoggerFactory.getLogger(DiscoveryController.class);

    @Autowired
    Service discoveryService;

    @ApiOperation(value = "Register an agent in the system")
    @PostMapping("/register")
    public void register(@RequestBody NewService service) {
        log.info("Register operation with {}", service);
        discoveryService.register(service);
    }

    @ApiOperation(value = "Discover agents in the system by name")
    @GetMapping("/discover")
    public List<ServiceInstance> discover(@RequestParam String name) {
        log.info("Discover operation with {}", name);
        return discoveryService.discover(name);
    }
}