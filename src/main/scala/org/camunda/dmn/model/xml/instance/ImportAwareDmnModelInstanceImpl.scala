/*
 * Copyright © 2022 Camunda Services GmbH (info@camunda.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.camunda.dmn.model.xml.instance

import org.camunda.bpm.model.dmn.impl.DmnModelInstanceImpl
import org.camunda.bpm.model.dmn.impl.DmnModelConstants
import org.camunda.bpm.model.xml.impl.ModelImpl
import org.camunda.bpm.model.xml.{ModelBuilder, ModelException}
import org.camunda.bpm.model.xml.instance.DomDocument
import org.camunda.bpm.model.xml.instance.ModelElementInstance
import org.camunda.bpm.model.dmn.instance.{DrgElement, NamedElement, Variable}
import org.camunda.bpm.model.xml.`type`.ModelElementType
import org.slf4j.LoggerFactory

import java.util
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

object ImportAwareDmnModelInstanceImpl {

  val dmnNameSpaces: List[String] = List(
    DmnModelConstants.DMN12_NS,
    DmnModelConstants.DMN12_NS,
    DmnModelConstants.DMN13_NS,
    DmnModelConstants.DMN11_ALTERNATIVE_NS,
    DmnModelConstants.DMN13_ALTERNATIVE_NS
  ).map(_.toLowerCase())

    /**
   * Allows an element's name to be qualified by the (import) name of the model that defines the element
   *
   * @param modelElement
   */
  implicit class ModelQualifiedElementName(modelElement: NamedElement) {
    def qualifiedName: String = {
      ((modelElement.getModelInstance, modelElement) match {
        case (m: ImportAwareDmnModelInstanceImpl, e: DrgElement) => m.importedModelName
        case (m: ImportAwareDmnModelInstanceImpl, v: Variable) => m.importedModelName
        case (_, _) => None
      }) match {
        case Some(qualifier) => s"$qualifier.${modelElement.getName}"
        case None => modelElement.getName
      }
    }
  }
}

/**
 * A dmn model instance that is able to resolve elements existing in the DRG that were imported from other dmn models.
 */

class ImportAwareDmnModelInstanceImpl (model: ModelImpl,
                                       modelBuilder: ModelBuilder,
                                       document: DomDocument,
                                       private val dmnModelInstanceProvider: DmnModelInstanceProvider)
  extends DmnModelInstanceImpl(model, modelBuilder, document) {
  import ImportAwareDmnModelInstanceImpl._

  private var importedModelName: Option[String] = None

  def withImportedModelName(modelName: Option[String]): ImportAwareDmnModelInstanceImpl = {
    val copy = clone()
    copy.importedModelName = modelName;
    copy
  }

  /**
   * The collection of imported models to be loaded once when required
   */
  private val importedModels = loadImports

  private def loadImports = getDefinitions.getImports.asScala
    .filter(id => dmnNameSpaces.contains(id.getImportType.toLowerCase()))
    .flatMap(id =>
      Try(dmnModelInstanceProvider.loadModel(id.getNamespace, Option(id.getLocationUri))
        .map {
          case iam: ImportAwareDmnModelInstanceImpl =>
            iam.withImportedModelName(Option(id.getAttributeValue("name")))
          case other => other
        }) match {
      case Success(m) => m
      case Failure(exception) =>
        val errorMsg = s"Unable to load imported model at location ${id.getLocationUri} " +
          s"for name ${id.getAttributeValue("name")}"
        LoggerFactory.getLogger(getClass).error(errorMsg, exception)
        throw new ModelException(errorMsg, exception)
    }).toSeq

  /**
   * Gets a model element from the model or an imported model whose id matches the given id
   *
   * @param id
   * @tparam T
   * @return
   */
  override def getModelElementById[T <: ModelElementInstance](id: String): T = {
    var element = super.getModelElementById[T](id)
    if (element == null) {
      importedModels.map(_.getModelElementById[T](id)).find(e => e != null) match {
        case Some(value) => element = value
        case None => //nothing to do
      }
    }
    element
  }

  /**
   * Gets all elements of the model and its imported models
   *
   * @param referencingClass - the type of element to be retrieved
   * @tparam T
   * @return
   */
  override def getModelElementsByType[T <: ModelElementInstance](referencingClass: Class[T]): util.Collection[T] = {
    (super.getModelElementsByType[T](referencingClass).asScala ++
      importedModels.flatMap(_.getModelElementsByType[T](referencingClass).asScala)).asJavaCollection
  }

  override def getModelElementsByType(`type`: ModelElementType): util.Collection[ModelElementInstance] = {
    (super.getModelElementsByType(`type`).asScala ++
      importedModels.flatMap(_.getModelElementsByType(`type`).asScala)).asJavaCollection
  }
  override def clone(): ImportAwareDmnModelInstanceImpl = {
    val superClone = super.clone()
    new ImportAwareDmnModelInstanceImpl(model, modelBuilder, superClone.getDocument, dmnModelInstanceProvider)
  }

}
