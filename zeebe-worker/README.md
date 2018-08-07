# DMN engine - Zeebe Worker

A [Zeebe](https://zeebe.io/) worker for decision evaluation (e.g. Business Rule Tasks).

## How to use it?

Download the [jar file](https://github.com/camunda/dmn-scala/releases) _(dmn-engine-zeebe-worker-${version}-full.jar)_.

Create a repository for your decisions (default is 'dmn-repo').

Run the application with

```
java -jar dmn-engine-zeebe-worker-${VERSION}-full.jar 
```

Per default, the application uses the directory 'dmn-repo' as repository. 

You can change the configuration by the following environment variables:

* `dmn.repo` - the directory the repository
* `zeebe.client.broker.contactPoint` - the Zeebe connection
* `zeebe.client.topic` - the Zeebe topic to work on

### Zeebe Task Definition

* register for jobs of type `DMN`
* required task header `decisionRef` => id of the decision to evaluate
* completes job with payload `result` which contains the complete decision result

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

You can build the project with [SBT](http://www.scala-sbt.org) or [Maven](http://maven.apache.org).

### Using SBT

In the root directory:

Run the tests with
```
sbt zeebeWorker/test
```

Build the jar including all dependencies with
```
sbt zeebeWorker/assembly
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

