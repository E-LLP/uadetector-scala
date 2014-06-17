/**
 * Copyright 2014 André Rouél
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.github.before.uadetector

import scala.Vector
import scala.io.Codec
import scala.io.Source
import scalaz.concurrent.Task
import scalaz.std.vector.vectorMonoid
import scalaz.stream.Process
import scalaz.stream.io
import com.github.before.uadetector.datasource.Data

object task {

  def createSource(file: String): Task[Source] = Task.delay {
    Source.fromFile(file)(Codec.UTF8)
  }

  def createSource(uri: java.net.URI): Task[Source] = Task.delay {
    Source.fromFile(uri)(Codec.UTF8)
  }

  def createSource(url: java.net.URL): Task[Source] = createSource(url.toURI)

  def loadData(readLines: Process[Task, String]): Process[Task, Data] = Process.eval {
    for (lines <- readLines.runFoldMap(Vector(_)))
      yield Data.load(lines)
  }

  def loadData(readLines: Task[Seq[String]]): Task[Data] =
    for (lines <- readLines)
      yield Data.load(lines)

  def loadDataFromSource(createSource: Task[Source]): Task[Data] =
    for {
      src <- createSource
      data <- loadData(io.linesR(src).runLog)
    } yield data

  def loadParser(loadData: Task[Data]): Task[Parser] =
    for (data <- loadData)
      yield new Parser(data)

  def loadParserFromSource(createSource: Task[Source]): Task[Parser] =
    for {
      data <- loadDataFromSource(createSource)
    } yield new Parser(data)

}