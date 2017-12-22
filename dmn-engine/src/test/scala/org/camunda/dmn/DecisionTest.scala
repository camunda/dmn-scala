package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.parser.ParsedDmn

trait DecisionTest {
  
  val engine = new DmnEngine
  
  def parse(file: String): ParsedDmn = {
    val stream = getClass.getResourceAsStream(file)    
    val result = engine.parse(stream)
    
    result.left.foreach(println)
        
    result.right.get
  }
  
  def eval(decision: ParsedDmn, id: String, context: Map[String, Any]): Any = {
    val result = engine.eval(decision, id, context)
    
    result.left.foreach(println)
    
    result.right.get
  }
  
}