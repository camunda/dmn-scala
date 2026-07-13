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

trait StackSizeTestSupport {

  val smallStackSize: Long = 512 * 1024

  // Runs `body` on a dedicated thread with an explicit stack size, so that
  // whether a StackOverflowError is thrown does not depend on the JVM's or
  // CI runner's default thread stack size.
  def runWithStackSize[T](stackSizeBytes: Long)(body: => T): T = {
    @volatile var outcome: Option[Either[Throwable, T]] = None

    val thread = new Thread(
      null,
      () => {
        outcome = Some(try {
          Right(body)
        } catch {
          case t: Throwable => Left(t)
        })
      },
      "stack-size-test-thread",
      stackSizeBytes
    )

    thread.start()
    try {
      thread.join()
    } catch {
      case _: InterruptedException =>
        Thread.currentThread().interrupt()
        throw new RuntimeException("interrupted while waiting for stack-size test thread")
    }

    outcome.getOrElse(
      Left(new IllegalStateException("thread completed without setting outcome"))) match {
      case Right(value) => value
      case Left(error)  => throw error
    }
  }

}
