# DMN Scala

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.camunda.bpm.extension.dmn.scala/dmn-engine/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.camunda.bpm.extension.dmn.scala/dmn-engine)

An engine to execute decisions according to the [DMN specification](http://www.omg.org/spec/DMN/About-DMN/) that is written in Scala. It uses the [FEEL-Scala engine](https://github.com/camunda/feel-scala) to evaluate FEEL expressions.

The DMN engine started as a slack time project, grown into a community-driven project, and is now officially maintained by [Camunda](https://camunda.org/). :rocket: 

It is integrated into [Camunda 8](https://github.com/camunda/camunda) to evaluate DMN decisions.

**Features:** ✨
* Support for the latest version of DMN (Compliance Level 3)
* Pluggable auditing for history or validation
* Extensible by custom functions and object mappers

The coverage of the DMN standard is measured by the [DMN TCK](https://dmn-tck.github.io/tck/index.html).

## Usage

Please have a look at Camunda's [DMN documentation](https://docs.camunda.io/docs/components/modeler/dmn/) and [FEEL documentation](https://docs.camunda.io/docs/components/modeler/feel/what-is-feel/). The documentation describes how to model DMN decisions and write FEEL expressions (e.g. data types, language constructs, builtin-functions, etc.).

## Install

Add the DMN engine as a dependency to your project.

```xml
<dependency>
  <groupId>org.camunda.bpm.extension.dmn.scala</groupId>
  <artifactId>dmn-engine</artifactId>
  <version>${version}</version>
</dependency>
```

### Custom Functions

The engine comes with a bunch of [built-in functions](https://docs.camunda.io/docs/components/modeler/feel/builtin-functions/feel-built-in-functions-introduction/) which are defined by the DMN specification. 
It allows you to define your own functions as FEEL expressions (using the keyword `function`) or as context entry element.

However, the engine provides also an SPI to add custom functions that are written in Scala / Java. The classes are loaded via Java's service loader mechanism. Please have a look at the [documentation](https://camunda.github.io/feel-scala/docs/reference/developer-guide/function-provider-spi) to see how to implement the SPI.

### Custom Object Mapper

The engine has a transformer (aka `ValueMapper`) to transform the incoming variables into FEEL types and to transform the decision result back into regular Scala types. 

If you need to transform custom types or change the result types then you can implement a SPI. The implementation is loaded via Java's service loader mechanism. Please have a look at the 
[documentation](https://camunda.github.io/feel-scala/docs/reference/developer-guide/value-mapper-spi) to see how to implement the SPI.

## Contribution

Found a bug? Please report it using [Github Issues](https://github.com/camunda/dmn-scala/issues).

Want to extend, improve or fix a bug? [Pull Requests](https://github.com/camunda/dmn-scala/pulls) are very welcome.

Want to discuss something? Take a look at the [Camunda Forum](https://forum.camunda.io/tag/dmn-engine-feel).

## License

[Apache License, Version 2.0](./LICENSE)
