package com.github.before.uadetector

import java.util.regex.Pattern
import java.util.regex.Pattern.CASE_INSENSITIVE
import java.util.regex.Pattern.DOTALL
import scala.collection.immutable.Map
import scala.collection.immutable.Vector
import org.specs2.mutable.Specification
import org.specs2._
import com.github.before.uadetector.task._
import com.github.before.uadetector.datasource._
import com.github.before.uadetector.datasource.IniFormat._
import scalaz.concurrent.Task
import scalaz.stream.Process
import net.sf.uadetector.UserAgentStringParser
import net.sf.uadetector.parser.UpdatingUserAgentStringParserImpl
import net.sf.uadetector.datastore.CachingXmlDataStore
import net.sf.uadetector.service.UADetectorServiceFactory
import net.sf.uadetector.service.UADetectorServiceFactory.ResourceModuleXmlDataStore
import net.sf.uadetector.datastore.OnlineXmlDataStore
import net.sf.uadetector.datastore.AbstractDataStore
import net.sf.uadetector.datareader.DataReader
import net.sf.uadetector.datareader.XmlDataReader
import java.net.URL
import scala.io.Codec
import java.nio.charset.Charset
import net.sf.uadetector.datastore.SimpleXmlDataStore
import scalaz.stream.io

import com.github.before.uadetector.datasource.IniFormat._

import ExampleLoader._

class ParserSpec extends Specification {

  "A parser" should {
    "hold an unknown data version if the given data does not provide a version" in {
      new Parser(Map()).version must be equalTo UnknownDataVersion
      new Parser(Map(Global -> Vector())).version must be equalTo UnknownDataVersion
    }
    "hold the data version of the given data source" in {
      val version = datasource.IniFormat.Version("20140619-01")
      new Parser(Map(Global -> Vector(version))).version must be equalTo version
    }
  }

  val version = "20140609-03"
  val deviceExamplesFile = s"uaDEVICE_example_${version}.csv"
  val osExamplesFile = s"uasOS_example_${version}.csv"
  val uasExamplesFile = s"uas_example_${version}.csv"

  s"All samples of $deviceExamplesFile, $osExamplesFile and $uasExamplesFile in version $version" should {

    "be parsed as expected" in {

      val deviceExamplesSrc = createSource(getClass().getClassLoader().getResource(deviceExamplesFile))
      val osExamplesSrc = createSource(getClass().getClassLoader().getResource(osExamplesFile))
      val uasExamplesSrc = createSource(getClass().getClassLoader().getResource(uasExamplesFile))
      val uasIniFile = getClass().getClassLoader().getResource(s"uas_${version}.ini").getFile
      val uasIniSrc = createSource(uasIniFile)

      def failedExamplesAsString(failedExamples: Task[Vector[String]]): Task[String] = {
        val prefix = "\n+ "
        for (failed <- failedExamples) yield {
          if (failed.isEmpty)
            ""
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

      failedExamplesAsString(failedUserAgentExamples).run must beEmpty
    }
  }

}
