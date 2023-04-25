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

import org.camunda.bpm.model.dmn.impl.{DmnModelInstanceImpl, DmnParser}
import org.camunda.bpm.model.dmn.{Dmn, DmnModelInstance}
import org.camunda.bpm.model.xml.impl.ModelImpl
import org.camunda.bpm.model.xml.instance.DomDocument

import java.io.InputStream

/**
 * A DmnModelInstanceProvider that maintains a list of all models that it has already been used to load,
 * which can then be retrieved by namespace
 */
class StatefulDmnModelInstanceProvider extends DmnParser with DmnModelInstanceProvider {

  val loadedModels: scala.collection.mutable.Map[String, DmnModelInstance] = scala.collection.mutable.Map.empty

  /**
   * Retrieves the dmn model
   * @param namespace - the namespace of the dmn model searched for
   * @param locator - a hint for the provider about where the model might be found. In this case, returns none if locator is not an InputStream
   *  @return - a DmnModelInstance if one was located for the given namespace, or None if the model could not be found
   */
  override def loadModel[T](namespace: String, locator: Option[T] = None): Option[DmnModelInstance] = {
   if (!loadedModels.contains(namespace)) {
     None
    } else {
     loadedModels.get(namespace)
   }
  }

  override def readModelFromStream(is: InputStream): DmnModelInstance = {
    Option(parseModelFromStream(is))
      .map(modelInstance => {
        loadedModels.put(
          modelInstance.getDefinitions.getNamespace,
          modelInstance)
        modelInstance
      }).orNull
  }

  override def createModelInstance(document: DomDocument): DmnModelInstanceImpl = {
    new ImportAwareDmnModelInstanceImpl(
      Dmn.INSTANCE.getDmnModel.asInstanceOf[ModelImpl],
      Dmn.INSTANCE.getDmnModelBuilder,
      document,
      this)
  }
}
