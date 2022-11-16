# DMN Scala

An engine to execute decisions according to the [DMN specification](http://www.omg.org/spec/DMN/About-DMN/). 

The engine uses the [Camunda DMN model api](https://github.com/camunda/camunda-dmn-model) for parsing DMN files and the [FEEL-Scala engine](https://github.com/camunda/feel-scala) to evaluate FEEL expressions.

**Features:**
* support for the latest version of DMN (Compliance Level 3)¹
  * Decision Table
  * Business Knowledge Model
  * Context
  * Literal Expression
  * List
  * Relation
  * Function Definition
  * Invocation
* evaluation of parsed DMN models
* extensible by own functions and types

¹ the DMN coverage is measured by the [DMN TCK](https://dmn-tck.github.io/tck/index.html)

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

### Provide Custom Functions

The engine comes with a bunch of [built-in functions](https://camunda.github.io/feel-scala/feel-built-in-functions) which are defined by the DMN specification. 
And it allows you to define own functions as FEEL expressions (using the keyword `function`) or as context entry element.

However, the engine provides also a SPI to add custom functions which are written in Scala / Java. The classes are loaded via Java's service loader mechanism. Please have a look at the [documentation](https://camunda.github.io/feel-scala/docs/reference/developer-guide/function-provider-spi) to see how to implement the SPI.

### Transform Custom Types

The engine has a transformer (aka `ValueMapper`) to transform the incoming variables into FEEL types and to transform the decision result back into regular Scala types. 

If you need to transform custom types or change the result types then you can implement a SPI. The implementation is loaded via Java's service loader mechanism. Please have a look at the 
[documentation](https://camunda.github.io/feel-scala/docs/reference/developer-guide/value-mapper-spi) to see how to implement the SPI.

## Contribution

Found a bug? Please report it using [Github Issues](https://github.com/camunda/dmn-scala/issues).

Want to extend, improve or fix a bug? [Pull Requests](https://github.com/camunda/dmn-scala/pulls) are very welcome.

Want to discuss something? The [Camunda Forum](https://forum.camunda.org/c/community-extensions) might be the best place for it.

## License

[Apache License, Version 2.0](./LICENSE)
