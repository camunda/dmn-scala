# DMN engine - Benchmark

Benchmark of the DMN engine using [OpenJDK JMH](http://openjdk.java.net/projects/code-tools/jmh/).

## How to run it?

You can run the benchmark with [SBT](http://www.scala-sbt.org).

### Using SBT

In the root directory:

Run the benchmark with
```
sbt benchmark/jmh:run -rf json -rff target/jmh-result.json
```
