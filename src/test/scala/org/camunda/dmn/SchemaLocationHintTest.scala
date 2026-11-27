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
package org.camunda.dmn

import org.camunda.dmn.DmnEngine.Failure
import org.camunda.dmn.parser.ParsedDmn
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files

class SchemaLocationHintTest extends AnyFlatSpec with Matchers {

  private val engine = new DmnEngine

  private def parse(dmn: String): Either[Failure, ParsedDmn] =
    engine.parse(new ByteArrayInputStream(dmn.getBytes(StandardCharsets.UTF_8)))

  private def writeSchema(xsd: String): URI = {
    val schema = Files.createTempFile("schema", ".xsd")
    schema.toFile.deleteOnExit()
    Files.write(schema, xsd.getBytes(StandardCharsets.UTF_8))
    schema.toUri
  }

  "A DMN model" should "ignore a schema location hint of the DMN namespace" in {
    val schema = writeSchema(
      """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
        |           targetNamespace="https://www.omg.org/spec/DMN/20191111/MODEL/"
        |           elementFormDefault="qualified">
        |  <xs:element name="definitions">
        |    <xs:complexType>
        |      <xs:attribute name="required" type="xs:string" use="required"/>
        |    </xs:complexType>
        |  </xs:element>
        |</xs:schema>""".stripMargin)

    val result = parse(
      s"""<?xml version="1.0" encoding="UTF-8"?>
         |<definitions xmlns="https://www.omg.org/spec/DMN/20191111/MODEL/"
         |             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         |             xsi:schemaLocation="https://www.omg.org/spec/DMN/20191111/MODEL/ $schema"
         |             id="definitions" name="definitions" namespace="http://camunda.org/schema/1.0/dmn">
         |  <decision id="greeting" name="Greeting">
         |    <literalExpression><text>"Hello " + name</text></literalExpression>
         |  </decision>
         |</definitions>""".stripMargin)

    withClue("Expected the model to parse: the schema in its schemaLocation rejects it, but only the bundled DMN schemas should count. ") {
      result.left.toOption shouldBe None
    }
  }

  it should "ignore a schema location hint of the DMNDI namespace" in {
    val schema = writeSchema(
      """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
        |           targetNamespace="https://www.omg.org/spec/DMN/20191111/DMNDI/"
        |           elementFormDefault="qualified">
        |  <xs:element name="DMNDI">
        |    <xs:complexType>
        |      <xs:attribute name="required" type="xs:string" use="required"/>
        |    </xs:complexType>
        |  </xs:element>
        |</xs:schema>""".stripMargin)

    val result = parse(
      s"""<?xml version="1.0" encoding="UTF-8"?>
         |<definitions xmlns="https://www.omg.org/spec/DMN/20191111/MODEL/"
         |             xmlns:dmndi="https://www.omg.org/spec/DMN/20191111/DMNDI/"
         |             xmlns:dc="http://www.omg.org/spec/DMN/20180521/DC/"
         |             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         |             xsi:schemaLocation="https://www.omg.org/spec/DMN/20191111/DMNDI/ $schema"
         |             id="definitions" name="definitions" namespace="http://camunda.org/schema/1.0/dmn">
         |  <decision id="greeting" name="Greeting">
         |    <literalExpression><text>"Hello " + name</text></literalExpression>
         |  </decision>
         |  <dmndi:DMNDI>
         |    <dmndi:DMNDiagram id="diagram">
         |      <dmndi:DMNShape id="shape" dmnElementRef="greeting">
         |        <dc:Bounds height="80" width="180" x="160" y="100"/>
         |      </dmndi:DMNShape>
         |    </dmndi:DMNDiagram>
         |  </dmndi:DMNDI>
         |</definitions>""".stripMargin)

    withClue("Expected the model to parse: the schema in its schemaLocation rejects it, but only the bundled DMN schemas should count. ") {
      result.left.toOption shouldBe None
    }
  }

  it should "ignore a schema location hint of a vendor namespace" in {
    val schema = writeSchema(
      """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
        |           targetNamespace="http://vendor.example.com/extension"
        |           elementFormDefault="qualified">
        |  <xs:element name="extension">
        |    <xs:complexType>
        |      <xs:attribute name="required" type="xs:string" use="required"/>
        |    </xs:complexType>
        |  </xs:element>
        |</xs:schema>""".stripMargin)

    val result = parse(
      s"""<?xml version="1.0" encoding="UTF-8"?>
         |<definitions xmlns="https://www.omg.org/spec/DMN/20191111/MODEL/"
         |             xmlns:vendor="http://vendor.example.com/extension"
         |             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         |             xsi:schemaLocation="http://vendor.example.com/extension $schema"
         |             id="definitions" name="definitions" namespace="http://camunda.org/schema/1.0/dmn">
         |  <decision id="greeting" name="Greeting">
         |    <extensionElements><vendor:extension/></extensionElements>
         |    <literalExpression><text>"Hello " + name</text></literalExpression>
         |  </decision>
         |</definitions>""".stripMargin)

    withClue("Expected the model to parse: the schema in its schemaLocation rejects it, but only the bundled DMN schemas should count. ") {
      result.left.toOption shouldBe None
    }
  }

  it should "ignore a schema location hint on a nested element" in {
    val schema = writeSchema(
      """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
        |           targetNamespace="http://vendor.example.com/extension"
        |           elementFormDefault="qualified">
        |  <xs:element name="extension">
        |    <xs:complexType>
        |      <xs:attribute name="required" type="xs:string" use="required"/>
        |    </xs:complexType>
        |  </xs:element>
        |</xs:schema>""".stripMargin)

    val result = parse(
      s"""<?xml version="1.0" encoding="UTF-8"?>
         |<definitions xmlns="https://www.omg.org/spec/DMN/20191111/MODEL/"
         |             id="definitions" name="definitions" namespace="http://camunda.org/schema/1.0/dmn">
         |  <decision id="greeting" name="Greeting"
         |            xmlns:vendor="http://vendor.example.com/extension"
         |            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         |            xsi:schemaLocation="http://vendor.example.com/extension $schema">
         |    <extensionElements><vendor:extension/></extensionElements>
         |    <literalExpression><text>"Hello " + name</text></literalExpression>
         |  </decision>
         |</definitions>""".stripMargin)

    withClue("Expected the model to parse: the schema in its schemaLocation rejects it, but only the bundled DMN schemas should count. ") {
      result.left.toOption shouldBe None
    }
  }

  "A DMN model that breaks the bundled DMN schema" should "be rejected" in {
    val result = parse(
      """<?xml version="1.0" encoding="UTF-8"?>
        |<definitions xmlns="https://www.omg.org/spec/DMN/20191111/MODEL/"
        |             id="definitions" name="definitions">
        |  <decision id="greeting" name="Greeting">
        |    <literalExpression><text>"Hello " + name</text></literalExpression>
        |  </decision>
        |</definitions>""".stripMargin)

    result.left.map(_.message) shouldBe Left(
      "Failed to parse DMN: org.camunda.bpm.model.dmn.DmnModelException: Unable to parse model")
  }

  it should "be rejected with a schema location hint to a schema that accepts it" in {
    val schema = writeSchema(
      """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
        |           targetNamespace="https://www.omg.org/spec/DMN/20191111/MODEL/"
        |           elementFormDefault="qualified">
        |  <xs:element name="definitions">
        |    <xs:complexType>
        |      <xs:sequence>
        |        <xs:any processContents="skip" minOccurs="0" maxOccurs="unbounded"/>
        |      </xs:sequence>
        |      <xs:anyAttribute processContents="skip"/>
        |    </xs:complexType>
        |  </xs:element>
        |</xs:schema>""".stripMargin)

    val result = parse(
      s"""<?xml version="1.0" encoding="UTF-8"?>
         |<definitions xmlns="https://www.omg.org/spec/DMN/20191111/MODEL/"
         |             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         |             xsi:schemaLocation="https://www.omg.org/spec/DMN/20191111/MODEL/ $schema"
         |             id="definitions" name="definitions">
         |  <decision id="greeting" name="Greeting">
         |    <literalExpression><text>"Hello " + name</text></literalExpression>
         |  </decision>
         |</definitions>""".stripMargin)

    withClue("Expected the model to be rejected: the schema in its schemaLocation accepts it, but only the bundled DMN schemas should count. ") {
      result.left.map(_.message) shouldBe Left(
        "Failed to parse DMN: org.camunda.bpm.model.dmn.DmnModelException: Unable to parse model")
    }
  }

  it should "be rejected with a schema location hint of the DC namespace to a schema that accepts it" in {
    val schema = writeSchema(
      """<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
        |           targetNamespace="http://www.omg.org/spec/DMN/20180521/DC/"
        |           elementFormDefault="qualified">
        |  <xs:element name="Bounds">
        |    <xs:complexType>
        |      <xs:attribute name="x" type="xs:double"/>
        |      <xs:attribute name="y" type="xs:double"/>
        |      <xs:attribute name="width" type="xs:double"/>
        |      <xs:attribute name="height" type="xs:double"/>
        |    </xs:complexType>
        |  </xs:element>
        |</xs:schema>""".stripMargin)

    val result = parse(
      s"""<?xml version="1.0" encoding="UTF-8"?>
         |<definitions xmlns="https://www.omg.org/spec/DMN/20191111/MODEL/"
         |             xmlns:dmndi="https://www.omg.org/spec/DMN/20191111/DMNDI/"
         |             xmlns:dc="http://www.omg.org/spec/DMN/20180521/DC/"
         |             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         |             xsi:schemaLocation="http://www.omg.org/spec/DMN/20180521/DC/ $schema"
         |             id="definitions" name="definitions" namespace="http://camunda.org/schema/1.0/dmn">
         |  <decision id="greeting" name="Greeting">
         |    <literalExpression><text>"Hello " + name</text></literalExpression>
         |  </decision>
         |  <dmndi:DMNDI>
         |    <dmndi:DMNDiagram id="diagram">
         |      <dmndi:DMNShape id="shape" dmnElementRef="greeting">
         |        <dc:Bounds width="180" x="160" y="100"/>
         |      </dmndi:DMNShape>
         |    </dmndi:DMNDiagram>
         |  </dmndi:DMNDI>
         |</definitions>""".stripMargin)

    withClue("Expected the model to be rejected: the schema in its schemaLocation accepts it, but only the bundled DMN schemas should count. ") {
      result.left.map(_.message) shouldBe Left(
        "Failed to parse DMN: org.camunda.bpm.model.dmn.DmnModelException: Unable to parse model")
    }
  }

  "A DMN model with a no-namespace schema location hint" should "be evaluated" in {
    val result = parse(
      """<?xml version="1.0" encoding="UTF-8"?>
        |<definitions xmlns="https://www.omg.org/spec/DMN/20191111/MODEL/"
        |             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |             xsi:noNamespaceSchemaLocation="https://www.omg.org/spec/DMN/20191111/DMN13.xsd"
        |             id="definitions" name="definitions" namespace="http://camunda.org/schema/1.0/dmn">
        |  <decision id="greeting" name="Greeting">
        |    <literalExpression><text>"Hello " + name</text></literalExpression>
        |  </decision>
        |</definitions>""".stripMargin)

    result.left.toOption shouldBe None
    engine
      .eval(result.toOption.get, "greeting", Map("name" -> "DMN"))
      .map(_.value) shouldBe Right("Hello DMN")
  }

  "A DMN 1.1 model with a schema location hint" should "be evaluated" in {
    val result = parse(
      """<?xml version="1.0" encoding="UTF-8"?>
        |<definitions xmlns="http://www.omg.org/spec/DMN/20151101/dmn.xsd"
        |             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |             xsi:schemaLocation="http://www.omg.org/spec/DMN/20151101/dmn.xsd http://www.omg.org/spec/DMN/20151101/dmn.xsd"
        |             id="definitions" name="definitions" namespace="http://camunda.org/schema/1.0/dmn">
        |  <decision id="greeting" name="Greeting">
        |    <literalExpression><text>"Hello " + name</text></literalExpression>
        |  </decision>
        |</definitions>""".stripMargin)

    result.left.toOption shouldBe None
    engine
      .eval(result.toOption.get, "greeting", Map("name" -> "DMN"))
      .map(_.value) shouldBe Right("Hello DMN")
  }

  "A DMN 1.2 model with a schema location hint" should "be evaluated" in {
    val result = parse(
      """<?xml version="1.0" encoding="UTF-8"?>
        |<definitions xmlns="http://www.omg.org/spec/DMN/20180521/MODEL/"
        |             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |             xsi:schemaLocation="http://www.omg.org/spec/DMN/20180521/MODEL/ http://www.omg.org/spec/DMN/20180521/DMN12.xsd"
        |             id="definitions" name="definitions" namespace="http://camunda.org/schema/1.0/dmn">
        |  <decision id="greeting" name="Greeting">
        |    <literalExpression><text>"Hello " + name</text></literalExpression>
        |  </decision>
        |</definitions>""".stripMargin)

    result.left.toOption shouldBe None
    engine
      .eval(result.toOption.get, "greeting", Map("name" -> "DMN"))
      .map(_.value) shouldBe Right("Hello DMN")
  }

  "A DMN 1.3 model with a schema location hint" should "be evaluated" in {
    val result = parse(
      """<?xml version="1.0" encoding="UTF-8"?>
        |<definitions xmlns="https://www.omg.org/spec/DMN/20191111/MODEL/"
        |             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |             xsi:schemaLocation="https://www.omg.org/spec/DMN/20191111/MODEL/ https://www.omg.org/spec/DMN/20191111/DMN13.xsd"
        |             id="definitions" name="definitions" namespace="http://camunda.org/schema/1.0/dmn">
        |  <decision id="greeting" name="Greeting">
        |    <literalExpression><text>"Hello " + name</text></literalExpression>
        |  </decision>
        |</definitions>""".stripMargin)

    result.left.toOption shouldBe None
    engine
      .eval(result.toOption.get, "greeting", Map("name" -> "DMN"))
      .map(_.value) shouldBe Right("Hello DMN")
  }

  "A DMN 1.4 model with a schema location hint" should "be evaluated" in {
    val result = parse(
      """<?xml version="1.0" encoding="UTF-8"?>
        |<definitions xmlns="https://www.omg.org/spec/DMN/20211108/MODEL/"
        |             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |             xsi:schemaLocation="https://www.omg.org/spec/DMN/20211108/MODEL/ https://www.omg.org/spec/DMN/20211108/DMN14.xsd"
        |             id="definitions" name="definitions" namespace="http://camunda.org/schema/1.0/dmn">
        |  <decision id="greeting" name="Greeting">
        |    <literalExpression><text>"Hello " + name</text></literalExpression>
        |  </decision>
        |</definitions>""".stripMargin)

    result.left.toOption shouldBe None
    engine
      .eval(result.toOption.get, "greeting", Map("name" -> "DMN"))
      .map(_.value) shouldBe Right("Hello DMN")
  }

  "A DMN 1.5 model with a schema location hint" should "be evaluated" in {
    val result = parse(
      """<?xml version="1.0" encoding="UTF-8"?>
        |<definitions xmlns="https://www.omg.org/spec/DMN/20230324/MODEL/"
        |             xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |             xsi:schemaLocation="https://www.omg.org/spec/DMN/20230324/MODEL/ https://www.omg.org/spec/DMN/20230324/DMN15.xsd"
        |             id="definitions" name="definitions" namespace="http://camunda.org/schema/1.0/dmn">
        |  <decision id="greeting" name="Greeting">
        |    <literalExpression><text>"Hello " + name</text></literalExpression>
        |  </decision>
        |</definitions>""".stripMargin)

    result.left.toOption shouldBe None
    engine
      .eval(result.toOption.get, "greeting", Map("name" -> "DMN"))
      .map(_.value) shouldBe Right("Hello DMN")
  }
}
