/*
 * Copyright (C) 2020  Atos Spain SA. All rights reserved.
 * 
 * This file is part of the FAIR4Health Service Discovery.
 * 
 * AuthTest.java is free software: you can redistribute it and/or modify it under the 
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
 * Spring boot application test services for FAIR4Health Service Discovery
 */

package eu.fair4health.discovery;

import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.hamcrest.Matchers;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.context.annotation.Bean;
import org.springframework.test.context.junit4.SpringRunner;

import eu.fair4health.discovery.service.DiscoveryService;

@RunWith(SpringRunner.class)
public class DiscoveryServiceTest {

    /**
     * Create the Bean Calculator Service.
     * 
     * @return the Calculator Service
     */
    /* @TestConfiguration
    static class DiscoveryServiceImplTestContextConfiguration {

        @Bean
        public DiscoveryService discoveryService() {
            return new DiscoveryService();
        }
    } */

    @InjectMocks
    private DiscoveryService discoveryService;
    
    /**
     * SetUp the system to mock the services.
     * 
     * @return void
     */
    @Before
    public void setUp() {
    }

    /**
     * Test all the Service functionalities provided.
     * 
     * @return void
     * @throws NotAuthorizedException, when some not authorization raises 
     */
    @Test
    public void callService_returnExpectedBehaviourTest() {

        // int found = discoveryService.register(1, 2);

        // assertThat(found, Matchers.<Integer> is(3));
    }

}
