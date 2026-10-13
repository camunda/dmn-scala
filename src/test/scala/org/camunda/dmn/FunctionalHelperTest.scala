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
import org.camunda.dmn.FunctionalHelper._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FunctionalHelperTest extends AnyFlatSpec with Matchers {

  "mapEither" should "map all elements in order" in {
    mapEither(List(1, 2, 3), (i: Int) => Right(i * 10)) should be(
      Right(List(10, 20, 30)))
  }

  it should "return the first failure" in {
    val result = mapEither(
      List(1, 2, 3),
      (i: Int) => if (i >= 2) Left(Failure(s"failed on $i")) else Right(i))

    result should be(Left(Failure("failed on 2")))
  }
}
