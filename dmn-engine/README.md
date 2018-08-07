# DMN Engine

An engine to evaluate DMN decisions written in Scala. 

## How to use it?

Add the DMN engine to your project by copying the [jar file](https://github.com/camunda/dmn-scala/releases) _(dmn-engine-${version}.jar)_ or adding the project as dependency.

```xml
<dependency>
  <groupId>org.camunda.bpm.extension.dmn.scala</groupId>
  <artifactId>dmn-engine</artifactId>
  <version>${version}</version>
</dependency>
```

Create a new instance of the class 'DmnEngine'. 
Use this instance to parse and evaluate a DMN decision. 

```scala
object MyProgram {
  
  val engine = new DmnEngine
  
  def evaluate(stream: InputStream, decisionId: String, context: Map[String, Any]) {
    
  val result = 
    engine
      .parse(stream)
      .flatMap(dmn => engine.eval(dmn, decisionId, context))
        
    result match {
      case Left(failure)    => // ...
      case Right(NilResult) => // ...
      case Right(r)         => // ...
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
