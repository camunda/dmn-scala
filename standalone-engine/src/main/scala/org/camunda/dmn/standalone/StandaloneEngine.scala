package org.camunda.dmn.standalone

import org.camunda.dmn.DmnEngine
import org.camunda.dmn.DmnEngine._
import java.io.InputStream

object StandaloneEngine {
  
  def main(args: Array[String]) 
  {
    val repositoryDirectory = args.toList match {
      case Nil => "decision-repository"
      case directory :: _ => directory
    }
    
    fileSystemRepository(repositoryDirectory)
  }
  
  def fileSystemRepository(directory: String): StandaloneEngine = 
  {
    val engine = new DmnEngine
    val repository = new FileSystemRepository(engine, directory)
    
    new StandaloneEngine(engine, repository)
  }
  
}

class StandaloneEngine(engine: DmnEngine, repository: DecisionRepository) {
  
  repository.init
  
  def evalDecisionById(id: String, variables: Map[String, Any]): Either[Failure, EvalResult] = 
  {
    repository.getDecisionById(id)
      .map(d => engine.eval(d.parsedDmn, id, variables))
      .getOrElse(Left(Failure(s"No decision found with id '$id'")))
  }
  
  def evalDecisionByName(name: String, variables: Map[String, Any]): Either[Failure, EvalResult] = 
  {
    repository.getDecisionByName(name)
      .map(d => engine.evalByName(d.parsedDmn, name, variables))
      .getOrElse(Left(Failure(s"No decision found with name '$name'")))
  }
  
  def getDecisions: List[DeployedDecision] = repository.getDecisions
  
  def insertDecisions(stream: InputStream, resource: String): Either[Failure, List[DeployedDecision]] = repository.insertDecisions(stream, resource)

  def removeResource(resource: String): Either[Failure, List[DeployedDecision]] = repository.removeResource(resource)
  
}