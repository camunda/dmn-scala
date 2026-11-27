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
package org.camunda.dmn.parser

import javax.xml.XMLConstants
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.Source
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.{Schema, SchemaFactory}
import org.camunda.bpm.model.dmn.impl.{DmnParser => CamundaDmnParser}

/** Reads DMN models like `Dmn.readModelFromStream`, but validates them only
  * against the DMN schemas bundled with camunda-dmn-model. It ignores the
  * `xsi:schemaLocation` and `xsi:noNamespaceSchemaLocation` hints in a model.
  */
private[parser] class BundledSchemaDmnParser extends CamundaDmnParser {

  import BundledSchemaDmnParser._

  override protected def configureFactory(
      factory: DocumentBuilderFactory): Unit = {
    super.configureFactory(factory)

    val bundledSchemaLocations =
      factory.getAttribute(JaxpSchemaSource).asInstanceOf[Array[String]]
    factory.setAttribute(JaxpSchemaLanguage, null)
    factory.setAttribute(JaxpSchemaSource, null)
    // Turns off DTD validation only. The Schema below validates against the bundled XSDs.
    factory.setValidating(false)
    factory.setSchema(compile(bundledSchemaLocations))

    try {
      factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "")
    } catch {
      case _: IllegalArgumentException => // Apache Xerces 2.x lacks this JAXP 1.5 property
    }
  }
}

private[parser] object BundledSchemaDmnParser {

  private val JaxpSchemaLanguage =
    "http://java.sun.com/xml/jaxp/properties/schemaLanguage"
  private val JaxpSchemaSource =
    "http://java.sun.com/xml/jaxp/properties/schemaSource"

  private def compile(locations: Array[String]): Schema = {
    val sources: Array[Source] =
      locations.map(location => new StreamSource(location))
    SchemaFactory
      .newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
      .newSchema(sources)
  }
}
