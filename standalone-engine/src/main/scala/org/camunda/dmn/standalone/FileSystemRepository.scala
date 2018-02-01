package org.camunda.dmn.standalone

import org.camunda.dmn._
import org.camunda.dmn.DmnEngine
import org.camunda.dmn.DmnEngine.Failure
import org.camunda.dmn.parser.ParsedDmn
import org.camunda.bpm.model.dmn.instance.Decision
import scala.collection.mutable
import scala.util.Try
import java.io.{InputStream, FileInputStream, IOException}
import java.nio.file.{Paths, Files}
import java.nio.file.StandardCopyOption
import java.io.ByteArrayOutputStream
import java.util.stream.Streams
import java.io.ByteArrayInputStream
import java.io.OutputStream

class FileSystemRepository(val dmnEngine: DmnEngine, directory: String) extends DecisionRepository {

  val path = Paths.get(directory)
   
  val deployedDecisions: mutable.Map[String, DeployedDecision] = mutable.Map.empty
  
  def init = scanDirectory
  
  private def scanDirectory 
  {
    logger.debug(s"Scan directory for decisions '$directory'")
    
    deployedDecisions.clear()
    
    try {
      Files.walk(path)
        .filter(p => p.getFileName.toString.endsWith(".dmn"))
        .forEach(p => 
        {
          parseDecision(new FileInputStream(p.toFile()), p.getFileName.toString)
            .right
            .map(decisions => deployedDecisions ++= decisions)
            .left
            .map(f => logger.warn(f.toString))
        })
    } catch {
      case e: IOException => logger.warn(s"Fail to scan directory: $directory", e)
    }
  }

  def getDecisions: List[DeployedDecision] = deployedDecisions.values.toList

  def getDecisionById(id: String): Option[DeployedDecision] = deployedDecisions.get(id)
  
  def getDecisionByName(name: String): Option[DeployedDecision] =
  {
    deployedDecisions.values
      .find(_.decisionName == name)
  }

  def insertDecisions(inputStream: InputStream, resource: String): Either[Failure, List[DeployedDecision]] = 
  {
    val resourceName = if (resource.endsWith(".dmn")) {
      resource
    } else {
      resource + ".dmn"
    }
    
    // need to copy input stream for parsing and copy
    val outputStream = copyStream(inputStream)
    
    val inputStreamParse = new ByteArrayInputStream(outputStream.toByteArray())
    parseDecision(inputStreamParse, resourceName)
      .right
      .flatMap(decisions => try 
      {
    	  val inputStreamCopy = new ByteArrayInputStream(outputStream.toByteArray())
        Files.copy(inputStreamCopy, path.resolve(resourceName), StandardCopyOption.REPLACE_EXISTING)
        
        Right(decisions)
      } catch {
        case t: Throwable => Left(Failure(s"Fail to copy resource '$resource': $t"))
      })
      .right
      .map(decisions => 
      {
        deployedDecisions ++= decisions 
        
        decisions.map{ case (id, d) => d}
      })
  }

  private def copyStream(input: InputStream): ByteArrayOutputStream = 
  {
    val output = new ByteArrayOutputStream
    
    val buffer = new Array[Byte](1024)
    var len = 0
    
    while (len > -1) {
      len = input.read(buffer)
      if (len > 0) {
        output.write(buffer, 0, len)
      }
    }
    output.flush()
    
    output
  }
  
  def removeResource(resource: String): Either[Failure, List[DeployedDecision]] = 
  {
    deployedDecisions.values.filter(_.resource == resource) match {
      case Nil => Left(Failure(s"No decisions found for resource '$resource'"))
      case decisionsToRemove =>
      {
        try {
          Files.delete(path.resolve(resource))
          
          Right(decisionsToRemove)
        } catch {
          case t: Throwable => Left(Failure(s"Fail to delete resource '$resource': $t"))
        }
      }
      .right
      .map(decisionsToRemove => 
      {
        val ids = decisionsToRemove.map(_.decisionId)
        
        deployedDecisions --= ids
        
        decisionsToRemove.toList
      })
    }
  }

}