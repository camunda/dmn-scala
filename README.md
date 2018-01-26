# DMN Scala

An engine to execute decisions according to the [DMN 1.1 specification](http://www.omg.org/spec/DMN/About-DMN/). 

**Features:**
* full support for DMN 1.1 (Compliance Level 3)¹
* ...

¹ measured by the DMN TCK: https://dmn-tck.github.io/tck/ (TODO: update results)

## Supported Elements

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
| List | - | - | x | 
| Function Definition | - | - | x | 
