# DMN Scala

An engine to execute decisions according to the [DMN 1.1 specification](http://www.omg.org/spec/DMN/About-DMN/). 

The engine uses the [Camunda DMN model api](https://github.com/camunda/camunda-dmn-model) for parsing DMN files and the [FEEL-Scala engine](https://github.com/camunda/feel-scala) to evaluate FEEL expressions.

**Features:**
* full support for DMN 1.1 (Compliance Level 3)¹
* evaluation of parsed DMN models
* extensible by own functions and types

¹ measured by the DMN TCK: https://dmn-tck.github.io/tck/ 

## How to use it?

You can use the DMN engine in different ways 

* [embedded](https://github.com/camunda/dmn-scala/tree/master/dmn-engine#how-to-use-it) as a library into your application
* [standalone](https://github.com/camunda/dmn-scala/tree/master/standalone-engine#how-to-use-it) via REST endpoint
* [integrated into the Camunda BPM engine](https://github.com/camunda/dmn-scala/tree/master/camunda-plugin#how-to-use-it) via process engine plugin
* [as a worker for Zeebe](https://github.com/camunda/dmn-scala/tree/master/zeebe-worker#how-to-use-it)

## Supported DMN Elements

Root Elements are
* Decision
* Business Knowledge Model

| ↓ element can be used for → | Decision | Business Knowledge Model | Context Entry |
| --- | --- | --- | --- |
| Decision Table | x | x | x | 
| Literal Expression | x | x | x | 
| Invocation | x | - | x | 
| Context | x | x | x | 
| Relation | x | x | x | 
| List | x | x | x | 
| Function Definition | - | - | x | 

## Extensions

You can customize the DMN engine in the following ways.

### Provide Custom Functions

The engine comes with a bunch of [built-in functions](https://github.com/camunda/feel-scala/wiki/FEEL-Builtin-Functions) which are defined by the DMN specification. 
And it allows you to define own functions as FEEL expressions (using the keyword `function`) or as context entry element.

However, the engine provides also a SPI to add custom functions which are written in Scala / Java. The classes are loaded via Java's service loader mechanism. Please have a look at the [Wiki](https://github.com/camunda/feel-scala/wiki/Function-Provider-SPI) to see how to implement the SPI.

### Transform Custom Types

The engine has a transformer (aka `ValueMapper`) to transform the incoming variables into FEEL types and to transform the decision result back into regular Scala types. 

If you need to transform custom types or change the result types then you can implement a SPI. The implementation is loaded via Java's service loader mechanism. Please have a look at the 
[Wiki](https://github.com/camunda/feel-scala/wiki/Value-Mapper-SPI) to see how to implement the SPI.

## Contribution

Found a bug? Please report it using [Github Issues](https://github.com/camunda/dmn-scala/issues).

Want to extend, improve or fix a bug? [Pull Requests](https://github.com/camunda/dmn-scala/pulls) are very welcome.

Want to discuss something? The [Camunda Forum](https://forum.camunda.org/c/community-extensions) might be the best place for it.

## License

[Apache License, Version 2.0](./LICENSE)
