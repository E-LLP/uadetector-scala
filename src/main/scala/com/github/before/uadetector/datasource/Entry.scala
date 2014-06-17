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
package com.github.before.uadetector.datasource

import com.github.before.uadetector.DeviceClass
import com.github.before.uadetector.datasource.regex.Pattern

/** Defines an entry of a series within a `Group` of an *UAS* data source */
trait Entry

case class Browser(
  id: BrowserId,
  typeId: Int,
  name: String,
  url: String,
  company: String,
  companyUrl: String,
  iconName: String,
  infoUrl: String) extends Entry

case class BrowserType(
  id: Int,
  name: String) extends Entry

case class BrowserRegex(
  id: Int,
  regex: Pattern,
  browserId: BrowserId) extends Entry

case class BrowserToOperatingSystem(
  browserId: BrowserId,
  operatingSystemId: OperatingSystemId) extends Entry

case class Device(
  id: DeviceId,
  name: String,
  iconName: String,
  infoUrl: String) extends Entry with DeviceClass

case class DeviceRegex(
  id: Int,
  regex: Pattern,
  deviceId: DeviceId) extends Entry

case class FailureType(msg: String) extends Entry

case class OperatingSystem(
  id: OperatingSystemId,
  family: String,
  name: String,
  url: String,
  company: String,
  companyUrl: String,
  iconName: String) extends Entry

case class OperatingSystemRegex(
  id: Int,
  regex: Pattern,
  operatingSystemId: OperatingSystemId) extends Entry

case class Robot(
  id: RobotId,
  userAgentString: String,
  family: String,
  name: String,
  url: String,
  company: String,
  companyUrl: String,
  iconName: String,
  osId: Option[OperatingSystemId],
  infoUrl: String) extends Entry
