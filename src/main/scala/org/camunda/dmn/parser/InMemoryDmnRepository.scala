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

import org.camunda.dmn.DmnEngine.Failure

import scala.collection.mutable

class InMemoryDmnRepository extends DmnRepository {

  private val parsedDmnByNamespace = mutable.Map.empty[String, ParsedDmn]

  override def getBusinessKnowledgeModel(namespace: String, bkmId: String): Either[Failure, ParsedBusinessKnowledgeModel] = {
    parsedDmnByNamespace.get(namespace) match {
      case None => Left(Failure(s"No BKM found with namespace '$namespace'."))
      case Some(parsedDmn) =>
        parsedDmn.bkms.find(_.id == bkmId) match {
          case None => Left(Failure(s"No BKM found with id '$bkmId' in namespace '$namespace'."))
          case Some(bkm) => Right(bkm)
        }
    }
  }

  override def getDecision(namespace: String, decisionId: String): Either[Failure, ParsedDecision] = {
    parsedDmnByNamespace.get(namespace) match {
      case None => Left(Failure(s"No decision found with namespace '$namespace'."))
      case Some(parsedDmn) =>
        parsedDmn.decisions.find(_.id == decisionId) match {
          case None => Left(Failure(s"No decision found with id '$decisionId' in namespace '$namespace'."))
          case Some(decision) => Right(decision)
        }
    }
  }

  override def put(dmn: ParsedDmn): Unit = {
    parsedDmnByNamespace.put(dmn.namespace, dmn)
  }
}
