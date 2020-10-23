/*
 * Copyright (C) 2020  Atos Spain SA. All rights reserved.
 * 
 * This file is part of the FAIR4Health Service Discovery project.
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
 * FAIR4Health Service Discovery interface
 */

package eu.fair4health.discovery.service;

import java.net.MalformedURLException;
import java.util.List;

import org.springframework.cloud.client.ServiceInstance;
import org.springframework.stereotype.Component;

import eu.fair4health.discovery.model.AgentService;

@Component
public interface Service {
	public void register(AgentService service) throws MalformedURLException;
        public List<ServiceInstance> discover(String serviceId);
        public List<ServiceInstance> discoverAll();
        public void deregister(String serviceId);
}

