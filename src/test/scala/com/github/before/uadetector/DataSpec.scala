package com.github.before.uadetector

import java.net.URL
import scala.collection.JavaConverters.asScalaSetConverter
import scala.collection.JavaConverters.mapAsScalaMapConverter
import scala.collection.immutable.Vector
import scala.io.Codec
import org.specs2.mutable.Specification
import com.github.before.uadetector.datasource.IniFormat.BrowserOperatingSystems
import com.github.before.uadetector.datasource.IniFormat.BrowserRegexs
import com.github.before.uadetector.datasource.IniFormat.BrowserTypes
import com.github.before.uadetector.datasource.IniFormat.Browsers
import com.github.before.uadetector.datasource.IniFormat.Comment
import com.github.before.uadetector.datasource.IniFormat.DeviceRegexs
import com.github.before.uadetector.datasource.IniFormat.Devices
import com.github.before.uadetector.datasource.IniFormat.OperatingSystemRegexs
import com.github.before.uadetector.datasource.IniFormat.OperatingSystems
import com.github.before.uadetector.datasource.IniFormat.Properties
import com.github.before.uadetector.datasource.IniFormat.Robots
import com.github.before.uadetector.datasource.IniFormat.Global
import net.sf.uadetector.datastore.SimpleXmlDataStore
import scalaz.stream.io
import com.github.before.uadetector.datasource.Entry

class DataSpec extends Specification {

  val version = "20140609-03"

  val uasData: URL = getClass().getClassLoader().getResource(s"uas_${version}.xml")
  val uasVersion: URL = getClass().getClassLoader().getResource(s"uas_${version}.version")
  val previousDataStructure = new SimpleXmlDataStore(uasData, uasVersion).getData()

  val resource = getClass().getClassLoader().getResource(s"uas_${version}.ini")
  val readLines = io.linesR(resource.getFile)(Codec.UTF8).runLog
  val newDataStructure = task.loadData(readLines).run

  "new data format" should {
    def comments(e: Entry): Boolean = e.isInstanceOf[Comment]
    def properties(e: Entry): Boolean = e.isInstanceOf[Properties]
    "read the same amount of devices" in {
      newDataStructure.getOrElse(Devices, Vector()) filter properties must beEmpty
      newDataStructure.getOrElse(Devices, Vector()).filterNot(comments(_)).size must be equalTo (previousDataStructure.getDevices.size)
    }
    "read the same amount of device patterns" in {
      newDataStructure.getOrElse(DeviceRegexs, Vector()) filter properties must beEmpty
      val previousSize = (for {
        entrySet <- previousDataStructure.getDevicePatterns.asScala
        pattern <- entrySet._2.asScala
      } yield pattern).size
      newDataStructure.getOrElse(DeviceRegexs, Vector()).filterNot(comments(_)).size must be equalTo previousSize
    }
    "read the same amount of browsers" in {
      newDataStructure.getOrElse(Browsers, Vector()) filter properties must beEmpty
      newDataStructure.getOrElse(Browsers, Vector()).filterNot(comments(_)).size must be equalTo (previousDataStructure.getBrowsers.size)
    }
    "read the same amount of browser patterns" in {
      newDataStructure.getOrElse(BrowserRegexs, Vector()) filter properties must beEmpty
      val previousSize = (for {
        entrySet <- previousDataStructure.getBrowserPatterns.asScala
        pattern <- entrySet._2.asScala
      } yield pattern).size
      newDataStructure.getOrElse(BrowserRegexs, Vector()).filterNot(comments(_)).size must be equalTo previousSize
    }
    "read the same amount of browser types" in {
      newDataStructure.getOrElse(BrowserTypes, Vector()) filter properties must beEmpty
      newDataStructure.getOrElse(BrowserTypes, Vector()).filterNot(comments(_)).size must be equalTo (previousDataStructure.getBrowserTypes.size)
    }
    "read the same amount of browser to OS mapping" in {
      newDataStructure.getOrElse(BrowserOperatingSystems, Vector()) filter properties must beEmpty
      newDataStructure.getOrElse(BrowserOperatingSystems, Vector()).filterNot(comments(_)).size must be equalTo (previousDataStructure.getBrowserToOperatingSystemMappings.size)
    }
    "read the same amount of operating systems" in {
      newDataStructure.getOrElse(OperatingSystems, Vector()) filter properties must beEmpty
      newDataStructure.getOrElse(OperatingSystems, Vector()).filterNot(comments(_)).size must be equalTo (previousDataStructure.getOperatingSystems.size)
    }
    "read the same amount of operating system patterns" in {
      newDataStructure.getOrElse(OperatingSystemRegexs, Vector()) filter properties must beEmpty
      val previousSize = (for {
        entrySet <- previousDataStructure.getOperatingSystemPatterns.asScala
        pattern <- entrySet._2.asScala
      } yield pattern).size
      newDataStructure.getOrElse(OperatingSystemRegexs, Vector()).filterNot(comments(_)).size must be equalTo previousSize
    }
    "read the same amount of robots" in {
      newDataStructure.getOrElse(Robots, Vector()) filter properties must beEmpty
      newDataStructure.getOrElse(Robots, Vector()).filterNot(comments(_)).size must be equalTo (previousDataStructure.getRobots.size)
    }
    "read the same version" in {
      import com.github.before.uadetector.datasource.IniFormat.Version
      newDataStructure.getOrElse(Global, Vector()).filterNot(comments(_)) must contain(Version(previousDataStructure.getVersion))
    }
  }

}