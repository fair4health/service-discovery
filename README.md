# FAIR4Health Service Discovery

# FAIR4Health Gateway

[![Build Status](https://travis-ci.org/fair4health/service-discovery.svg?branch=master)](https://travis-ci.org/fair4health/service-discovery) 
[![codecov.io](https://codecov.io/gh/fair4health/service-discovery/branch/master/graphs/badge.svg)](http://codecov.io/gh/fair4health/service-discovery)
[![Quality Gate](https://sonarcloud.io/api/project_badges/measure?project=eu.fair4health:service-discovery&metric=alert_status)](https://sonarcloud.io/dashboard/index/eu.fair4health:service-discovery)
[![Docker Build](https://img.shields.io/docker/cloud/build/fair4health/service-discovery)](https://cloud.docker.com/u/fair4health/repository/docker/fair4health/service-discovery)
[![Docker Pulls](https://img.shields.io/docker/pulls/fair4health/service-discovery)](https://cloud.docker.com/u/fair4health/repository/docker/fair4health/service-discovery)
[![License](https://img.shields.io/badge/License-Apache%202.0-green.svg)](https://opensource.org/licenses/Apache-2.0)

## Description

FAIR4Health Service Discovery to register and discover PPDDM Agents in [Consul](https://www.consul.io/):

- [POST] /discovery/register
- [GET]  /discovery/discover

## Technology

- Java 8+
- Maven for Java dependency management
- Spring Boot 
- Spring Cloud Consul
- 
- Lombok for the models

## Functionalities

- Service registry and discovery
- Metadata using K/V Storage

## How to deploy

Compile and package the project with

```
mvn clean package
```

and execute

```
java -jar target/service-discovery.war
```

It can also be run as:

```
mvn spring-boot:run
```

## Environment variables

    CONSUL_HOST=
    CONSUL_PORT=
    LOGGING_FOLDER=
    LOGGING_MODE=

## Docker deployment

Build the image:

```
docker build -t fair4health/service-discovery .
```

Simple deployment:

```
docker run --name discovery -d fair4health/service-discovery
```

Logging can be also configured using `LOGGING_FOLDER` and sharing a volume (this is useful for example for [ELK](https://www.elastic.co/elk-stack) processing). The level of the logging can be configured with `LOGGING_MODE` (dev|prod):

```
docker run --name gateway -d -v /tmp/log/service-discovery:/log/service-discovery -e LOGGING_FOLDER=/log/service-discovery -e LOGGING_MODE=dev fair4health/service-discovery
```

## License

Apache 2.0

By downloading this software, the downloader agrees with the specified terms and conditions of the License Agreement and the particularities of the license provided.
