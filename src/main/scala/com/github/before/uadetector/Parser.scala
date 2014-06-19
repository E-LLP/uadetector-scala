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

import scala.collection.immutable.Vector

import com.github.before.uadetector.datasource.Browser
import com.github.before.uadetector.datasource.BrowserId
import com.github.before.uadetector.datasource.BrowserRegex
import com.github.before.uadetector.datasource.BrowserToOperatingSystem
import com.github.before.uadetector.datasource.Data
import com.github.before.uadetector.datasource.DataVersion
import com.github.before.uadetector.datasource.Device
import com.github.before.uadetector.datasource.DeviceId
import com.github.before.uadetector.datasource.DeviceRegex
import com.github.before.uadetector.datasource.IniFormat.BrowserOperatingSystems
import com.github.before.uadetector.datasource.IniFormat.BrowserRegexs
import com.github.before.uadetector.datasource.IniFormat.BrowserTypes
import com.github.before.uadetector.datasource.IniFormat.Browsers
import com.github.before.uadetector.datasource.IniFormat.DeviceRegexs
import com.github.before.uadetector.datasource.IniFormat.Devices
import com.github.before.uadetector.datasource.IniFormat.Global
import com.github.before.uadetector.datasource.IniFormat.OperatingSystemRegexs
import com.github.before.uadetector.datasource.IniFormat.OperatingSystems
import com.github.before.uadetector.datasource.IniFormat.Robots
import com.github.before.uadetector.datasource.IniFormat.Section
import com.github.before.uadetector.datasource.OperatingSystemId
import com.github.before.uadetector.datasource.OperatingSystemRegex
import com.github.before.uadetector.datasource.Robot
import com.github.before.uadetector.datasource.UnknownDataVersion

class Parser(val data: Data) {

  val browserOperatingSystems = Parser.filterBySectionAndType(data, BrowserOperatingSystems, classOf[BrowserToOperatingSystem])
    .groupBy(_.browserId)
    .map {
      case (browserId, osIds) => (browserId, osIds(0).operatingSystemId)
    }

  val browserRegexs = Parser.filterBySectionAndType(data, BrowserRegexs, classOf[BrowserRegex])

  val browserTypes = Parser.filterBySectionAndType(data, BrowserTypes, classOf[datasource.BrowserType])
    .groupBy(_.id)
    .map {
      case (id, types) => (id, types(0))
    }

  val browsers = Parser.filterBySectionAndType(data, Browsers, classOf[Browser])
    .groupBy(_.id)
    .map {
      case (id, browsers) => (id, browsers(0))
    }

  val deviceRegexs = Parser.filterBySectionAndType(data, DeviceRegexs, classOf[DeviceRegex])

  val devices = Parser.filterBySectionAndType(data, Devices, classOf[Device])
    .groupBy(_.id)
    .map {
      case (id, devices) => (id, devices(0))
    }

  val operatingSystemRegexs = Parser.filterBySectionAndType(data, OperatingSystemRegexs, classOf[OperatingSystemRegex])

  val operatingSystems = Parser.filterBySectionAndType(data, OperatingSystems, classOf[datasource.OperatingSystem])
    .groupBy(_.id)
    .map {
      case (id, os) => (id, os(0))
    }

  val robots = Parser.filterBySectionAndType(data, Robots, classOf[Robot])

  lazy val version: DataVersion = Parser.filterBySectionAndType(data, Global, classOf[DataVersion])
    .headOption
    .getOrElse(UnknownDataVersion)

  private[uadetector] def browser(uaString: String): Option[(Browser, Option[Version])] = for {
    browserIdAndVersion <- browserId(uaString)
    browser <- browsers.get(browserIdAndVersion._1)
  } yield (browser, browserIdAndVersion._2)

  private[uadetector] def browserId(uaString: String): Option[(BrowserId, Option[Version])] = {
    for (browserRegex <- browserRegexs) {
      val matcher = browserRegex.regex.regex.matcher(uaString)
      if (matcher.find) {
        return Some(browserRegex.browserId, Version.browserVersion(matcher))
      }
    }
    None
  }

  private[uadetector] def operatingSystem(id: BrowserId, uaString: String): Option[OperatingSystem] = {
    for {
      osId <- browserOperatingSystems.get(id)
      os <- operatingSystems.get(osId)
    } yield OperatingSystem(
      os.family,
      os.iconName,
      os.name,
      os.company,
      os.companyUrl,
      os.url,
      Version.parseOperatingSystemVersion(os.family, uaString))
  }

  private[uadetector] def operatingSystem(uaString: String): Option[OperatingSystem] = {
    for {
      osId <- operatingSystemId(uaString)
      os <- operatingSystems.get(osId)
    } yield OperatingSystem(
      os.family,
      os.iconName,
      os.name,
      os.company,
      os.companyUrl,
      os.url,
      Version.parseOperatingSystemVersion(os.family, uaString))
  }

  private[uadetector] def operatingSystemId(uaString: String): Option[OperatingSystemId] = {
    for (osRegex <- operatingSystemRegexs) {
      val matcher = osRegex.regex.regex.matcher(uaString)
      if (matcher.find)
        return Some(osRegex.operatingSystemId)
    }
    None
  }

  private[uadetector] def userAgentType(id: Int): Option[UserAgentType] =
    for (t <- browserTypes.get(id)) yield UserAgentType.classify(t.name)

  private[uadetector] def device(uaString: String): Option[Device] = {
    // classification depends on matching order
    for (deviceRegex <- deviceRegexs) {
      val m = deviceRegex.regex.regex.matcher(uaString)
      if (m.find)
        return devices.get(deviceRegex.deviceId)
    }
    None
  }

  private[uadetector] def device(uaString: String, uaType: Option[UserAgentType]): Option[Device] = {

    if (uaType.isDefined) {

      // a robot will be classified as 'Other'
      if (uaType.get == RobotType) {
        return devices.get(DeviceId.others)
      }

      // classify
      val classification = device(uaString)
      if (classification.isDefined) {
        return classification
      }

    }

    // an unknown user agent type should lead to an unknown device
    if (uaType.isEmpty) {
      return None
    }

    if (uaType.isDefined) {
      val t = uaType.get

      // if no pattern is available but the type is Other, Library, Validator or UA Anonymizer
      // than classify it as 'Other'
      if (t == OtherType || t == Library || t == Validator || t == UserAgentAnonymizer) {
        return devices.get(DeviceId.others)
      }

      // if no pattern is available but the type is a mobile or WAP browser than classify it as 'Smartphone'
      if (t == MobileBrowser || t == WapBrowser) {
        return devices.get(DeviceId.smartphone)
      }

    }

    devices.get(DeviceId.personalComputer)
  }

  private[uadetector] def tryAsBrowser(uaString: String): Option[UserAgent] = {
    for (browserAndVersion <- browser(uaString)) {
      val browser = browserAndVersion._1
      val uaType = userAgentType(browser.typeId)
      val version = browserAndVersion._2
      return Some(UserAgent(
        device(uaString, uaType),
        browser.iconName,
        browser.name,
        operatingSystem(browser.id, uaString) orElse operatingSystem(uaString),
        browser.company,
        browser.companyUrl,
        uaType,
        browser.url,
        version))
    }
    None
  }

  private[uadetector] def tryAsRobot(uaString: String): Option[UserAgent] = {
    for (r <- robots) {
      if (r.userAgentString == uaString) {
        // try to get the version from the last found group
        val version = Version.parseLastVersionNumber(r.name)
        val uaType = Some(RobotType)
        return Some(UserAgent(
          device(uaString, uaType),
          r.iconName,
          r.family,
          None,
          r.company,
          r.companyUrl,
          uaType,
          r.url,
          version))
      }
    }
    None
  }

  private[uadetector] def tryDeviceAndOperatingSystem(uaString: String): Option[UserAgent] = {
    val deviceClass = device(uaString)
    val os = operatingSystem(uaString)
    if (deviceClass.isDefined || os.isDefined) {
      return Some(UserAgent(
        deviceClass,
        "",
        "",
        os,
        "",
        "",
        None,
        "",
        None))
    }
    None
  }

  /**
   * Parses an User-Agent string
   */
  def parse(uaString: String): Option[UserAgent] = {
    tryAsRobot(uaString) orElse
      tryAsBrowser(uaString) orElse
      tryDeviceAndOperatingSystem(uaString)
  }

}

object Parser {

  private[uadetector] def filterBySectionAndType[A](source: Data, section: Section, typ: Class[A]): Vector[A] = {
    val zero = Vector[A]()
    source.get(section).getOrElse(zero).foldLeft(zero) { (acc, e) =>
      if (typ.isAssignableFrom(e.asInstanceOf[AnyRef].getClass))
        acc :+ typ.cast(e)
      else
        acc
    }
  }

}
