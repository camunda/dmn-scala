# DMN engine - Zeebe Worker

A [Zeebe](https://zeebe.io/) worker for decision evaluation (e.g. Business Rule Tasks).

The application uses the [Spring-Zeebe integration](https://github.com/zeebe-io/spring-zeebe) (based on Spring Boot) to connect the DMN engine with Zeebe.

## How to use it?

Download the JAR file `dmn-engine-zeebe-worker-${VERSION}.jar`.

Create a repository for your decisions (default is 'dmn-repo').

Run the application with

```
java -Dzeebe.topic=default-topic -Dzeebe.lockOwner=dmn-scala -jar dmn-engine-zeebe-worker-${VERSION}.jar 
```

Per default, the application uses the directory 'dmn-repo' as repository. 

You can change the configuration by using the properties

```
java -Ddmn.repo=my-repo -Dzeebe.topic=my-topic -Dzeebe.lockOwner=my-owner -jar dmn-engine-zeebe-worker-${VERSION}.jar 
```

You can also set the configuration in a 'application.yaml' or 'application.properties' file.

### Zeebe Task Definition

* register for tasks of type 'DMN'
* required task header 'decisionRef' => id of the decision to evaluate
* completes task with payload 'result' which contains the complete decision result

```xml
<bpmn:serviceTask id="decisionTask" name="Eval DMN decision">
  <bpmn:extensionElements>
    <zeebe:taskDefinition type="DMN" />
    <zeebe:taskHeaders>
      <zeebe:header key="decisionRef" value="dish-decision" />
    </zeebe:taskHeaders>
    <zeebe:ioMapping>
      <zeebe:output source="$.result" target="$.decisionResult" />
    </zeebe:ioMapping>
  </bpmn:extensionElements>
</bpmn:serviceTask>
```

## How to build it?

You can build the project with [Maven](http://maven.apache.org).

Run the tests with
```
mvn test
```

Build the jar with
```
mvn install
```

