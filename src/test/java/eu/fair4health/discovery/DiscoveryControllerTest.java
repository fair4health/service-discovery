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

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

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
import eu.fair4health.discovery.service.DiscoveryService;

@SpringBootTest
@RunWith(SpringJUnit4ClassRunner.class)
public class DiscoveryControllerTest {
    private MockMvc mockMvc;

    @InjectMocks
    private DiscoveryController discoveryController;

    @Mock
    private DiscoveryService discoveryService;

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
     */
    @Before
    public void setUp() {
        List<ServiceInstance> result = new ArrayList<ServiceInstance>();
        Mockito.when(discoveryService.discover("test"))
            .thenReturn(result);
    }

    /**
     * Test the discover controller mocking the service.
     * 
     * @return void
     */
    @Test
    public void discoverServiceByName_returnOk() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.get("/discover?name=test"))
            .andExpect(status().isOk());
        // .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        // .andExpect(content().contentType("3"));
    }
}
