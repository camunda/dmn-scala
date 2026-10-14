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
package org.camunda.dmn.performance

import org.camunda.dmn.FunctionalHelper._
import org.scalatest.TryValues
import org.scalatest.concurrent.{Signaler, ThreadSignaler, TimeLimitedTests}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}

import scala.util.Try

// Performance test to detect an O(n^2) regression in FunctionalHelper.mapEither.
// camunda/camunda#62594
class MapEitherPerformanceTest
    extends AnyFlatSpec
    with Matchers
    with TryValues
    with TimeLimitedTests {

  // Each test must complete within the time limit; a quadratic regression
  // exceeds it. Interrupt the test thread on timeout.
  override val timeLimit = Span(10, Seconds)
  override val defaultTestSignaler: Signaler = ThreadSignaler

  "mapEither" should "build its result in linear time" in {
    // Appending to an immutable List copies it, so a result built by
    // appending is O(n^2): ~5 billion copied elements for 100,000 elements,
    // which takes seconds instead of milliseconds and exceeds the time limit.
    val elementCount = 100000

    // Wrap the build in Try so the ThreadSignaler can interrupt cleanly on
    // timeout instead of leaving the test thread hanging.
    val result = Try(mapEither(1 to elementCount, (i: Int) => Right(i)))

    result.success.value.map(_.size) should be(Right(elementCount))
  }
}
