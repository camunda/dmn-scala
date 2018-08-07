# DMN engine - REST

A standalone DMN engine with a REST endpoint.

## How to use it?

Download the [jar file](https://github.com/camunda/dmn-scala/releases) _(dmn-engine-rest-${version}-full.jar)_.

Create a repository for your decisions (default is 'dmn-repo').

Run the application with

```
java -jar dmn-engine-rest-${VERSION}-full.jar
```

Per default, the application starts on port 8080 and uses the directory 'dmn-repo' as repository. 

You can change the configuration by using the properties

```
java -Dport=8090 -Ddmn.repo=my-repo -jar dmn-engine-rest-${VERSION}-full.jar
```

### REST Endpoints


method | path | body | description
--- | --- | --- | ---
GET | /decisions | - | return the deployed decisions 
POST | /decisions/:id/eval | variables as JSON document | evaluate the decision with the given id 
POST | /decisions | files as multipart/form-data | deploy the given files 
DELETE | /decisions/:resource | - | remove all decisions related to the given resource

## How to build it?

You can build the project with [SBT](http://www.scala-sbt.org) or [Maven](http://maven.apache.org).

### Using SBT

In the root directory:

Run the server for development with

```
~;engineRest/jetty:stop;jetty:star
```

Run the tests with
```
sbt engineRest/test
```

Build the jar including all dependencies with
```
sbt engineRest/assembly
```

### Using Maven

Run the tests with
```
mvn test
```

Build the jar including all dependencies with
```
mvn install
```
