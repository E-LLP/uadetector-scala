package com.github.before.uadetector

import scala.Vector
import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.github.before.uadetector.datasource.IniFormat

import scalaz.concurrent.Task
import scalaz.std.vector.vectorMonoid
import scalaz.stream.Process
import scalaz.stream.io

object ExampleLoader {

  def unquote(s: String): String = IniFormat.Entry.unquote(s)

  type CheckedExamples = Vector[Try[UserAgent]]
  type DeviceExample = (String, String) // name, user agent string
  type OperatingSystemExample = (String, String) // name, user agent string
  type Pairs = Vector[(String, String)]
  type Triples = Vector[(String, String, String)]
  type UserAgentExample = (String, String, String) // type, name, user agent string

  def checkDeviceExamples(loadParser: Task[Parser], loadExamples: Task[Pairs]): Task[CheckedExamples] = {
    for {
      parser <- loadParser
      examples <- loadExamples
    } yield {
      val parsingTasks = examples map parseDeviceExamples(parser)
      for {
        parsingTask <- parsingTasks
      } yield parsingTask.run
    }
  }

  def checkOperatingSystemExamples(loadParser: Task[Parser], loadExamples: Task[Pairs]): Task[CheckedExamples] = {
    for {
      parser <- loadParser
      examples <- loadExamples
    } yield {
      val parsingTasks = examples map parseOperatingSystems(parser)
      for {
        parsingTask <- parsingTasks
      } yield parsingTask.run
    }
  }

  def checkUserAgentExamples(loadParser: Task[Parser], loadExamples: Task[Triples]): Task[CheckedExamples] = {
    for {
      parser <- loadParser
      examples <- loadExamples
    } yield {
      val parsingTasks = examples map parseUserAgentExamples(parser)
      for {
        parsingTask <- parsingTasks
      } yield parsingTask.run
    }
  }

  def loadOperatingSystems[A](loadSource: Task[Source])(f: Process[Task, String] => Task[A]): Task[A] = {
    for {
      src <- loadSource
      triples <- f(io.linesR(src))
    } yield triples
  }

  def fetchPairs: Process[Task, String] => Task[Pairs] = _
    .map(_.split(",")) // split to columns
    .map(arr => (unquote(arr(0)), unquote(arr.drop(1).mkString(",")))) // pick name and user agent string
    .runFoldMap { case (name, uaString) => Vector((name, uaString)) }

  def fetchTriples: Process[Task, String] => Task[Triples] = _
    .map(_.split(",")) // split to columns
    .map(arr => (unquote(arr(0)), unquote(arr(1)), unquote(arr.drop(2).mkString(",")))) // pick name and user agent string
    .runFoldMap { case (typeName, name, uaString) => Vector((typeName, name, uaString)) }

  def failingExamples(checkedExamples: Task[CheckedExamples]) = {
    for (checked <- checkedExamples) yield {
      checked
        .filter(_.isFailure)
        .map(_ match {
          case Failure(t) => t.getLocalizedMessage
          case _ => throw new Error("not possible")
        })
    }
  }

  def loadExamples[A](loadSource: Task[Source])(f: Process[Task, String] => Task[A]): Task[A] = {
    for {
      src <- loadSource
      triples <- f(io.linesR(src))
    } yield triples
  }

  def loadExamplesAsPairs(loadSource: Task[Source]): Task[Pairs] =
    loadExamples(loadSource)(fetchPairs)

  def loadExamplesAsTriples(loadSource: Task[Source]): Task[Triples] =
    loadExamples(loadSource)(fetchTriples)

  private def parseDeviceExamples(parser: Parser)(example: DeviceExample): Task[Try[UserAgent]] = Task.delay {
    val userAgent = parser.parse(example._2)
    if (userAgent.isDefined) {
      val ua = userAgent.get
      val d = ua.deviceCategory
      if (d.isEmpty)
        Failure(new IllegalStateException(s"no device class available (example: ${example})"))
      else if (d.get.name != example._1)
        Failure(new IllegalStateException(s"wrong device class (actual: ${d.get.name}, required: ${example._1}, example: ${example})"))
      else
        Success(ua)
    } else
      Failure(new IllegalStateException(s"device class sample could not be detected (example: ${example})"))
  }

  private def parseOperatingSystems(parser: Parser)(example: OperatingSystemExample): Task[Try[UserAgent]] = Task.delay {
    val userAgent = parser.parse(example._2)
    if (userAgent.isDefined) {
      val ua = userAgent.get
      val os = ua.operatingSystem
      if (os.isEmpty)
        Failure(new IllegalStateException(s"no operating system available (example: ${example})"))
      else if (os.get.name != example._1)
        Failure(new IllegalStateException(s"wrong operating system (actual: ${os.get.name}, required: ${example._1}, example: ${example})"))
      else
        Success(ua)
    } else
      Failure(new IllegalStateException(s"operating system sample could not be detected (example: ${example})"))
  }

  private def parseUserAgentExamples(parser: Parser)(example: UserAgentExample): Task[Try[UserAgent]] = Task.delay {
    val userAgent = parser.parse(example._3)
    if (userAgent.isDefined) {
      val ua = userAgent.get
      val t = ua.uaType
      if (t.isEmpty)
        Failure(new IllegalStateException(s"no type available (example: ${example})"))
      else if (t.get.name != example._1)
        Failure(new IllegalStateException(s"wrong type (actual: ${t.get.name}, required: ${example._1}, example: ${example})"))
      else if (ua.name != example._2)
        Failure(new IllegalStateException(s"wrong name (actual: ${ua.name}, required: ${example._2}, example: ${example})"))
      else
        Success(ua)
    } else
      Failure(new IllegalStateException(s"user agent sample could not be detected (example: ${example})"))
  }

}
