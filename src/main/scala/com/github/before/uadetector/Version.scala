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

import java.util.regex.Matcher
import java.util.regex.Pattern

case class Version(groups: Seq[String], extension: String) {
  def major: Option[String] = partAt(0)
  def minor: Option[String] = partAt(1)
  def revision: Option[String] = partAt(2)
  def partAt(index: Int) = if (groups.contains(index)) Some(groups(index)) else None
  override def toString(): String = groups.mkString(".") + extension
}

object Version {

  /**
   * Represents an empty or not set version number
   */
  val empty = Version(Nil, "")

  /**
   * Index number of the group in a matching {@link Pattern} which contains the extension/suffix of a version string
   */
  private val extensionIndex = 5

  /**
   * Index number of the group in a matching {@link Pattern} which contains the first/major number of a version string
   */
  private val majorIndex = 1

  /**
   * Regular expression to analyze a version number separated by a dot
   */
  private val versionNumber = Pattern.compile("((\\d+)((\\.\\d+)+)?)")

  /**
   * Regular expression to analyze a version number separated by a dot with suffix
   */
  private val versionNumberWithSuffix = Pattern.compile(versionNumber.pattern() + "((\\s|\\-|\\.|\\[|\\]|\\w+)+)?")

  /**
   * Regular expression to analyze segments of a version string, consisting of prefix, numeric groups and suffix
   */
  private val versionString = Pattern.compile("^" + versionNumberWithSuffix.pattern())

  /**
   * The number of capturing groups if nothing matches
   */
  val zeroMatchingGroups = 0

  private[uadetector] def browserVersion(matcher: Matcher): Option[Version] = {
    if (matcher.groupCount() > zeroMatchingGroups) {
      val versionString = if (matcher.group(1) != null) matcher.group(1) else ""
      return Version.parseVersion(versionString);
    }
    None
  }

  /**
   * This method try to determine the version number of the operating system <i>Android</i> more accurately.
   *
   * @param userAgent
   *            user agent string
   * @return more accurately identified version number or {@code null}
   */
  def identifyAndroidVersion(userAgent: String): Option[Version] = {
    for (pattern <- Patterns.Android) {
      val m = pattern.matcher(userAgent)
      if (m.find()) {
        return parseFirstVersionNumber(m.group(majorIndex))
      }
    }
    None
  }

  /**
   * This method try to determine the version number of the operating system <i>Bada</i> more accurately.
   *
   * @param userAgent
   *            user agent string
   * @return more accurately identified version number or {@code null}
   */
  def identifyBadaVersion(userAgent: String): Option[Version] = {
    val m = Patterns.Bada.matcher(userAgent)
    if (m.find()) {
      return parseFirstVersionNumber(m.group(majorIndex))
    }
    None
  }

  /**
   * This method try to determine the version number of an operating system of a <i>BSD</i> platform more accurately.
   *
   * @param userAgent
   *            user agent string
   * @return more accurately identified version number or {@code null}
   */
  def identifyBSDVersion(userAgent: String): Option[Version] = {
    val m = Patterns.BSD.matcher(userAgent)
    if (m.find()) {
      return parseFirstVersionNumber(m.group(majorIndex))
    }
    None
  }

  /**
   * This method try to determine the version number of the operating system <i>iOS</i> more accurately.
   *
   * @param userAgent
   *            user agent string
   * @return more accurately identified version number or {@code null}
   */
  def identifyIOSVersion(userAgent: String): Option[Version] = {
    for (pattern <- Patterns.iOS) {
      val m = pattern.matcher(userAgent)
      if (m.find()) {
        return parseFirstVersionNumber(m.group(majorIndex).replaceAll("_", "."))
      }
    }
    None
  }

  /**
   * This method try to determine the version number of the running <i>JVM</i> more accurately.
   *
   * @param userAgent
   *            user agent string
   * @return more accurately identified version number or {@code null}
   */
  def identifyJavaVersion(userAgent: String): Option[Version] = {
    for (pattern <- Patterns.Java) {
      val m = pattern.matcher(userAgent)
      if (m.find()) {
        return parseFirstVersionNumber(m.group(majorIndex))
      }
    }
    None
  }

  /**
   * This method try to determine the version number of the operating system <i>OS X</i> more accurately.
   *
   * @param userAgent
   *            user agent string
   * @return more accurately identified version number or {@code null}
   */
  def identifyOSXVersion(userAgent: String): Option[Version] = {
    for (pattern <- Patterns.OSX) {
      val m = pattern.matcher(userAgent)
      if (m.find()) {
        return parseFirstVersionNumber(m.group(majorIndex).replaceAll("_", "."))
      }
    }
    None
  }

  /**
   * This method try to determine the version number of the operating system <i>Symbian</i> more accurately.
   *
   * @param userAgent
   *            user agent string
   * @return more accurately identified version number or {@code null}
   */
  def identifySymbianVersion(userAgent: String): Option[Version] = {
    val m = Patterns.Symbian.matcher(userAgent)
    if (m.find()) {
      return parseFirstVersionNumber(m.group(majorIndex))
    }
    None
  }

  /**
   * This method try to determine the version number of the operating system <i>webOS</i> more accurately.
   *
   * @param userAgent
   *            user agent string
   * @return more accurately identified version number or {@code null}
   */
  def identifyWebOSVersion(userAgent: String): Option[Version] = {
    for (pattern <- Patterns.webOS) {
      val m = pattern.matcher(userAgent)
      if (m.find()) {
        return parseFirstVersionNumber(m.group(majorIndex))
      }
    }
    None
  }

  /**
   * This method try to determine the version number of the operating system <i>Windows</i> more accurately.
   *
   * @param userAgent
   *            user agent string
   * @return more accurately identified version number or {@code null}
   */
  def identifyWindowsVersion(userAgent: String): Option[Version] = {
    for (pattern <- Patterns.Windows) {
      val m = pattern.matcher(userAgent)
      if (m.find()) {
        return parseFirstVersionNumber(m.group(majorIndex))
      }
    }
    None
  }

  /**
   * Interprets a string with version information. The first occurrence of a version number in the string will be
   * searched and processed.
   *
   * @param text
   *            string with version information
   * @return an object of {@code VersionNumber}, never {@code null}
   */
  def parseFirstVersionNumber(text: String): Option[Version] = {
    val matcher = versionNumberWithSuffix.matcher(text)
    var split: Seq[String] = null
    var ext: String = null
    if (matcher.find()) {
      split = matcher.group(majorIndex).split("\\.")
      ext = matcher.group(extensionIndex)
    }
    val extension = if (ext == null) Version.empty.extension else trimRight(ext)
    if (split == null) None else Some(Version(split, extension))
  }

  /**
   * Interprets a string with version information. The last version number in the string will be searched and
   * processed.
   *
   * @param text
   *            string with version information
   * @return an object of {@code VersionNumber}, never {@code null}
   */
  def parseLastVersionNumber(text: String): Option[Version] = {
    val matcher = versionNumberWithSuffix.matcher(text)
    var split: Seq[String] = null
    var ext: String = null
    while (matcher.find()) {
      split = matcher.group(majorIndex).split("\\.")
      ext = matcher.group(extensionIndex)
    }
    val extension = if (ext == null) Version.empty.extension else trimRight(ext)
    if (split == null) None else Some(Version(split, extension))
  }

  /**
   * Try to determine the version number of the operating system by parsing the user agent string.
   *
   *
   * @param family
   *            family of the operating system
   * @param userAgent
   *            user agent string
   * @return extracted version number
   */
  def parseOperatingSystemVersion(family: String, userAgent: String): Option[Version] = family match {
    case "Android" => identifyAndroidVersion(userAgent)
    case "Bada" => identifyBadaVersion(userAgent)
    case "BSD" => identifyBSDVersion(userAgent)
    case "iOS" => identifyIOSVersion(userAgent)
    case "JVM" => identifyJavaVersion(userAgent)
    case "OS X" => identifyOSXVersion(userAgent)
    case "Symbian OS" => identifySymbianVersion(userAgent)
    case "webOS" => identifyWebOSVersion(userAgent)
    case "Windows" => identifyWindowsVersion(userAgent)
    case _ => None
  }

  /**
   * Interprets a string with version information. The first found group will be taken and processed.
   *
   * @param version
   *            version as string
   * @return an object of {@code VersionNumber}, never {@code null}
   */
  def parseVersion(version: String): Option[Version] = {
    val matcher = versionString.matcher(version)
    if (matcher.find()) {
      val groups = matcher.group(majorIndex).split("\\.")
      val extension = if (matcher.group(extensionIndex) == null) Version.empty.extension else trimRight(matcher.group(extensionIndex))
      return Some(Version(groups, extension))
    }
    None
  }

  /**
   * Trims the whitespace at the end of the given string.
   *
   * @param text
   *            string to trim
   * @return trimmed string
   */
  private def trimRight(text: String): String = text.replaceAll("\\s+$", "")

  object Patterns {
    val Android = List(
      Pattern.compile("Android\\s?((\\d+)((\\.\\d+)+)?(\\-(\\w|\\d)+)?);"),
      Pattern.compile("Android\\-((\\d+)((\\.\\d+)+)?(\\-(\\w|\\d)+)?);"))
    val Bada = Pattern.compile("Bada/((\\d+)((\\.\\d+)+)?)")
    val BSD = Pattern.compile("\\w+bsd\\s?((\\d+)((\\.\\d+)+)?((\\-|_)[\\w\\d\\-]+)?)", Pattern.CASE_INSENSITIVE)
    val iOS = List(
      Pattern.compile("iPhone OS\\s?((\\d+)((\\_\\d+)+)?) like Mac OS X"),
      Pattern.compile("CPU OS\\s?((\\d+)((\\_\\d+)+)?) like Mac OS X"),
      Pattern.compile("iPhone OS\\s?((\\d+)((\\.\\d+)+)?);"))
    val Java = List(
      Pattern.compile("Java/((\\d+)((\\.\\d+)+)?((\\-|_)[\\w\\d\\-]+)?)"),
      Pattern.compile("Java((\\d+)((\\.\\d+)+)?((\\-|_)[\\w\\d\\-]+)?)"))
    val OSX = List(
      Pattern.compile("Mac OS X\\s?((\\d+)((\\.\\d+)+)?);"),
      Pattern.compile("Mac OS X\\s?((\\d+)((\\_\\d+)+)?);"),
      Pattern.compile("Mac OS X\\s?((\\d+)((\\_\\d+)+)?)\\)"))
    val Symbian = Pattern.compile("SymbianOS/((\\d+)((\\.\\d+)+)?s?)")
    val webOS = List(
      Pattern.compile("hpwOS/((\\d+)((\\.\\d+)+)?);"),
      Pattern.compile("webOS/((\\d+)((\\.\\d+)+)?);"))
    val Windows = List(
      Pattern.compile("Windows NT\\s?((\\d+)((\\.\\d+)+)?)"),
      Pattern.compile("Windows Phone OS ((\\d+)((\\.\\d+)+)?)"),
      Pattern.compile("Windows CE ((\\d+)((\\.\\d+)+)?)"),
      Pattern.compile("Windows 2000\\s?((\\d+)((\\.\\d+)+)?)"),
      Pattern.compile("Windows XP\\s?((\\d+)((\\.\\d+)+)?)"),
      Pattern.compile("Windows 7\\s?((\\d+)((\\.\\d+)+)?)"),
      Pattern.compile("Win 9x ((\\d+)((\\.\\d+)+)?)"),
      Pattern.compile("Windows ((\\d+)((\\.\\d+)+)?)"),
      Pattern.compile("WebTV/((\\d+)((\\.\\d+)+)?)"))
  }

}
