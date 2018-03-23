# DMN Engine

An engine to evaluate DMN decisions written in Scala. 

## How to use it?

Add the DMN engine to your project by copying the [jar file](https://github.com/saig0/dmn-scala/releases) _(dmn-engine-${project.version}.jar)_ or adding the project as dependency.

```xml
<dependency>
  <groupId>org.camunda.bpm.extension.dmn.scala</groupId>
  <artifactId>dmn-engine</artifactId>
  <version>${project.version}</version>
</dependency>
```

Create a new instance of the class 'DmnEngine'. 
Use this instance to parse and evaluate a DMN decision. 

```scala
object MyProgram {
  
  val engine = new DmnEngine
  
  def feel(expression: String, context: Map[String, Any]) {
    
    val result: EvalResult = engine.evalSimpleUnaryTests(expression, context)
    
    result match {
      case EvalValue(value)   =>  // ...
      case EvalFailure(error) =>  // ...
      case ParseFailure(error) => // ...
    }
  }  
}
```

## How to build it?

You can build the project with [SBT](http://www.scala-sbt.org) or [Maven](http://maven.apache.org).

### Using SBT

In the root directory:

Run the tests with
```
sbt engine/test
```

### Using Maven

Run the tests with
```
mvn test
```
