package com.github.before.uadetector

import com.github.before.uadetector.ExampleLoader.checkDeviceExamples
import com.github.before.uadetector.ExampleLoader.checkOperatingSystemExamples
import com.github.before.uadetector.ExampleLoader.checkUserAgentExamples
import com.github.before.uadetector.ExampleLoader.failingExamples
import com.github.before.uadetector.ExampleLoader.loadExamplesAsPairs
import com.github.before.uadetector.ExampleLoader.loadExamplesAsTriples
import com.github.before.uadetector.task.createSource
import com.github.before.uadetector.task.loadParserFromSource

import scalaz.concurrent.Task

object ParseExamples extends App {

  val version = "20140612-01"

  val deviceExamplesFile = getClass().getClassLoader().getResource(s"uaDEVICE_example_${version}.csv")
  val deviceExamplesSrc = createSource(deviceExamplesFile)
  val osExamplesFile = getClass().getClassLoader().getResource(s"uasOS_example_${version}.csv")
  val osExamplesSrc = createSource(osExamplesFile)
  val uasExamplesFile = getClass().getClassLoader().getResource(s"uas_example_${version}.csv")
  val uasExamplesSrc = createSource(uasExamplesFile)
  val uasIniFile = getClass().getClassLoader().getResource(s"uas_${version}.ini").getFile
  val uasIniSrc = createSource(uasIniFile)

  def failedExamplesAsString(failedExamples: Task[Vector[String]]): Task[String] = {
    val prefix = "\n+ "
    for (failed <- failedExamples) yield {
      if (failed.isEmpty)
        "all examples passed successfully, nothing failed"
      else
        prefix + failed.mkString(prefix)
    }
  }

  val parser = loadParserFromSource(uasIniSrc)
  val deviceExamples = loadExamplesAsPairs(deviceExamplesSrc)
  val osExamples = loadExamplesAsPairs(osExamplesSrc)
  val userAgentExamples = loadExamplesAsTriples(uasExamplesSrc)
  val checkedUserAgentExamples = checkUserAgentExamples(parser, userAgentExamples)
  val checkedExamples = for {
    checkedDeviceExamples <- checkDeviceExamples(parser, deviceExamples)
    checkedOperatingSystemExamples <- checkOperatingSystemExamples(parser, osExamples)
    checkedUserAgentExamples <- checkUserAgentExamples(parser, userAgentExamples)
  } yield checkedDeviceExamples ++ checkedOperatingSystemExamples ++ checkedUserAgentExamples
  val failedUserAgentExamples = failingExamples(checkedExamples)

  val forPrinting = failedExamplesAsString(failedUserAgentExamples)

  val result = Time.measure(forPrinting.run)

  println(s"Executed in ${result._2.toMillis} milliseconds\n\t${result._1}")

}

