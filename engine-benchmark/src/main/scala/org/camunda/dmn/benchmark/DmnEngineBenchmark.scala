package org.camunda.dmn.benchmark

import org.camunda.dmn.DmnEngine
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

object DmnEngineBenchmark {
  
  val engine = new DmnEngine
              
  val parsedDmn = engine.parse(getClass.getResourceAsStream("/discount.dmn")).right.get
  
  val variables = Map("customer" -> "Business", "orderSize" -> 15)
  
}

@BenchmarkMode(Array(Mode.Throughput))
@Fork(1)
@Warmup(iterations = 20, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 20, time = 1, timeUnit = TimeUnit.SECONDS)
class DmnEngineBenchmark {
  
  import DmnEngineBenchmark._
    
  @Benchmark
  def evaluateDecisionTable()
  {
    engine.eval(parsedDmn, "discount", variables)
  }
  
}