/*
 * Copyright (C) 2020  All rights reserved.
 * 
 * This file is part of the FAIR4Health Service Discovery.
 * 
 * TodoApplicationControllerTest.java is free software: you can redistribute it and/or modify it under the 
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
 * FAIR4Health Service Discovery Spring boot Controller tests
 */
package eu.fair4health.discovery;

import static org.mockito.Mockito.mock;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import eu.fair4health.discovery.controller.DiscoveryController;
import eu.fair4health.discovery.model.AgentService;
import eu.fair4health.discovery.service.DiscoveryService;

@SpringBootTest
@RunWith(SpringJUnit4ClassRunner.class)
public class DiscoveryControllerTest {
    private MockMvc mockMvc;

    @InjectMocks
    private DiscoveryController discoveryController;

    @Mock
    private DiscoveryService discoveryService;

    private String authToken = "123456789";
    
    /**
     * Init the Mockito annotations and MVC mock.
     * 
     * @return void
     */
    @Before
    public void init() {
        MockitoAnnotations.initMocks(this);
        mockMvc = MockMvcBuilders.standaloneSetup(discoveryController)
            .build();
    }

    /**
     * Mock the service calls for the different operations.
     * 
     * @return void
     * @throws MalformedURLException 
     */
    @Before
    public void setUp() throws MalformedURLException {
        List<ServiceInstance> result = new ArrayList<ServiceInstance>();
        Mockito.when(discoveryService.discover("test"))
            .thenReturn(result);
        Mockito.when(discoveryService.discoverAll())
            .thenReturn(result);
        
        DiscoveryService myService = mock(DiscoveryService.class);
        Mockito.doNothing().when(myService).register(new AgentService());
    }

    /**
     * Test the discover controller mocking the service.
     * 
     * @return void
     */
    @Test
    public void discoverInstancesByName_returnOk() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.get("/discover?name=test")
            .header("Authorization", "Bearer " + authToken))
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
    }
    

    /**
     * Test the discover all instances controller mocking the service.
     * 
     * @return void
     */
    @Test
    public void discoverAllInstances_returnOk() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.get("/discover")
            .header("Authorization", "Bearer " + authToken))
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
    }
}
