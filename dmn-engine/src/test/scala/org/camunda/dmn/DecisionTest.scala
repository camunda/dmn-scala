package org.camunda.dmn

trait DecisionTest {
  
  val engine = new DmnEngine
  
  def parse(file: String): ParsedDmn = {
    val stream = getClass.getResourceAsStream(file)    
    val result = engine.parse(stream)
    
    if (result.isRight) {
      println(result.right.get.mkString("\n"))
    }
    
    result.left.get
  }
  
}