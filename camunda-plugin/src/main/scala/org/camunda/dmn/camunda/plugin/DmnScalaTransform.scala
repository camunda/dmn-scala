package org.camunda.dmn.camunda.plugin

import org.camunda.bpm.dmn.engine.impl.spi.transform.DmnTransformer
import org.camunda.bpm.dmn.engine.impl.spi.transform.DmnTransform
import java.io.InputStream
import java.io.File
import java.io.FileInputStream
import org.camunda.bpm.model.dmn.DmnModelInstance
import java.util.{List => JList}
import org.camunda.bpm.dmn.engine.DmnDecision
import org.camunda.bpm.dmn.engine.DmnDecisionRequirementsGraph
import org.camunda.commons.utils.EnsureUtil

class DmnScalaTransform(dmnEngine: CamundaDmnEngine) extends DmnTransform {

  var inputStream: InputStream = _

  override def transformDecisions[T <: DmnDecision](): JList[T] = {
    EnsureUtil.ensureNotNull("input stream", inputStream)

    dmnEngine.parseDecisions(inputStream).asInstanceOf[JList[T]]
  }

  override def transformDecisionRequirementsGraph[
      T <: DmnDecisionRequirementsGraph](): T = {
    EnsureUtil.ensureNotNull("input stream", inputStream)

    dmnEngine.parseDecisionRequirementsGraph(inputStream).asInstanceOf[T]
  }

  override def modelInstance(inputStream: InputStream) = {
    this.inputStream = inputStream
    this
  }

  override def setModelInstance(inputStream: InputStream) {
    modelInstance(inputStream)
  }

  override def modelInstance(file: File) = {
    modelInstance(new FileInputStream(file))
  }

  override def setModelInstance(file: File) {
    modelInstance(file)
  }

  ///// not supported

  override def modelInstance(modelInstance: DmnModelInstance) = {
    throw new UnsupportedOperationException()
  }

  override def setModelInstance(modelInstance: DmnModelInstance) {
    throw new UnsupportedOperationException()
  }
}
