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

import org.camunda.bpm.model.dmn.{Dmn, DmnModelInstance}
import org.camunda.bpm.model.xml.impl.ModelImpl

import java.io.InputStream
import java.net.URI
import scala.reflect.ClassTag.Nothing

object URILocatorDmnModelInstanceProvider {
  /**
   * Attempts to open an input stream on a resource at the given uri. If the uri is absolute, then an attempt is made
   * to open the stream at that location. If the URI is relative, then the resource is assumed to be a classpath
   * resource, and an attempt will be made to locate it on the classpath.
   *
   * @param uri - A java.net.URI suitable for locating a resource
   */
  implicit class URItoStream(val uri: URI) {
    def inputStream: InputStream = if (uri.isAbsolute) {
      uri.toURL.openStream()
    } else {
      ClassLoader.getSystemResourceAsStream(s"${uri.getPath}")
    }
  }
}

class URILocatorDmnModelInstanceProvider extends StatefulDmnModelInstanceProvider {
  import org.camunda.dmn.model.xml.instance.URILocatorDmnModelInstanceProvider._

  /**
   * Loads a model from a URI given by the locator, or returns a model already loaded
   *
   * @param namespace - the namespace of the dmn model searched for
   * @param locator - An absolute URI or a path to a location on the classpath where the model can be found
   * @tparam T
   *  @return - a DmnModelInstance if one was located for the given namespace, or None if the model could not be found
   */
  override def loadModel[T](namespace: String, locator: Option[T]): Option[DmnModelInstance] = {
    super.loadModel(namespace, locator)
      .orElse(locator.map {
        case s: String => readModelFromStream(URI.create(s).inputStream)
        case uri: URI => readModelFromStream(uri.inputStream)
        case _ => null
      })
  }
}
