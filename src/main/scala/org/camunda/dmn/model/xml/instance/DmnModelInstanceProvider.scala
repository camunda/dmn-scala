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

import jdk.dynalink.Namespace
import org.camunda.bpm.model.dmn.DmnModelInstance
import org.camunda.bpm.model.xml.ModelBuilder
import org.camunda.bpm.model.xml.impl.ModelImpl
import org.camunda.bpm.model.xml.instance.DomDocument

import java.io.InputStream

/**
 * Provides DmnModelInstances via an InputStream or through some other locator
 */
trait DmnModelInstanceProvider {

  /**
   * Attempts to find and load a dmn model for the given namespace, optionally
   * using the locator if necessary
   *
   * @param namespace - the namespace of the dmn model searched for
   * @param locator - a hint for the provider about where the model might be found
   * @return - a DmnModelInstance if one was located for the given namespace, or None if the model could not be found
   */
  def loadModel[T](namespace: String, locator: Option[T]): Option[DmnModelInstance]

  /**
   * Instantiates a DmnModelInstance from the given Input Stream
   *
   * @param is - the input stream from which a dmn model can be read
   * @return
   */
  def readModelFromStream(is: InputStream): DmnModelInstance
}
