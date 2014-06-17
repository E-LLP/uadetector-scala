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

import scala.annotation.tailrec
import scala.collection.immutable.Map
import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.github.before.uadetector.datasource.regex.Converter
import com.github.before.uadetector.datasource.regex.Pattern

/**
 * Describes the INI format of an *UAS* database (Data for UASparser) available
 * from http://user-agent-string.info.
 */
object IniFormat {

  sealed trait Line

  /** Determines the type of a given line in string representation. */
  object Line {
    def apply(line: String): Line = {
      if (Comment.classify(line))
        Comment.parse(line).get
      else if (Section.classify(line))
        Section.parse(line).get
      else if (Property.classify(line))
        Property(line)
      else
        Unknown(line)
    }
  }

  case class Comment(value: String) extends Line with Entry

  /** Semicolons (;) at the beginning of the line indicate a comment. */
  object Comment {
    def classify(line: String): Boolean =
      if (line.nonEmpty)
        line.head == ';'
      else
        false
    def parse(line: String): Option[Comment] =
      if (line.nonEmpty)
        Some(Comment(line.substring(1)))
      else
        None
  }

  case class Property(line: String = "") extends Line {
    private val index = line.indexOf('=')
    require(index > 0, "property must contain a name and value separated by an equal signs")
    private val split = line.splitAt(index)
    val name: String = split._1
    val value: String = split._2.substring(1)
  }

  /**
   * The basic element contained in an INI file is the *key* or *property*.
   * Every key has a *name* and a *value*, delimited by an equals sign (=). The
   * name appears to the left of the equals sign.
   */
  object Property {
    def classify(line: String): Boolean =
      if (line.indexOf('=') > 0)
        true
      else
        false
    def parse(line: String): Option[Property] = {
      if (classify(line))
        Some(Property(line))
      else
        None
    }
  }

  /**
   * An intermediate state when collecting properties until they can be
   * transformed into an `Entry`.
   */
  case class Properties(values: Vector[Property]) extends Entry // TODO reuse when folding list of lines to entries

  object Properties {
    import Entry._
    def convert(section: Section, entries: Vector[Property]): Try[Entry] = {
      if (enoughEntries(section, entries))
        section match {
          case Browsers =>
            mapToBrowser(entries)
          case BrowserOperatingSystems =>
            mapToBrowserToOperatingSystem(entries)
          case BrowserRegexs =>
            mapToBrowserRegex(entries)
          case BrowserTypes =>
            mapToBrowserType(entries)
          case Devices =>
            mapToDevice(entries)
          case DeviceRegexs =>
            mapToDeviceRegex(entries)
          case OperatingSystems =>
            mapToOperatingSystem(entries)
          case OperatingSystemRegexs =>
            mapToOperatingSystemRegex(entries)
          case Robots =>
            mapToRobot(entries)
          case u: UnknownSection =>
            Success(FailureType("Entries of unknown sections cannot be mapped"))
          case Global =>
            Success(FailureType("Entries in global section must be mapped separately"))
        }
      else
        Try {
          Properties(entries)
        }
    }
  }

  /**
   * Keys may (but need not) be grouped into arbitrarily named sections. The
   * section name appears on a line by itself, in square brackets (`[` and `]`).
   * All keys after the section declaration are associated with that section.
   * There is no explicit "end of section" delimiter; sections end at the next
   * section declaration, or the end of the file. Sections may not be nested.
   */
  sealed trait Section extends Line with Group {
    def isGlobal = false
    def isKnown = false
  }
  case object Global extends Section {
    override def isGlobal = true
  }
  case class UnknownSection(name: String) extends Section

  object Section {
    def classify(line: String): Boolean =
      if (line.nonEmpty)
        line.head == '[' && line.last == ']'
      else
        false
    def parse(line: String): Option[Section] =
      if (line.nonEmpty) {
        val name = line.substring(1, line.length - 1)
        val section = KnownSection.parse(name).getOrElse(UnknownSection(name))
        Some(section)
      } else
        None
  }

  /**
   * If a string could not be determined as `Comment`, `Section` or `Property`
   * it becomes an `Unknown` type.
   */
  case class Unknown(value: String) extends Line with Entry

  /** Defines known section of an *UAS* INI file */
  sealed trait KnownSection extends Section {
    def classify(line: String): Boolean =
      if (line.nonEmpty)
        line == id
      else
        false
    def id: String
    override def isKnown = true
  }
  object KnownSection {
    val variants = List(
      BrowserOperatingSystems,
      BrowserRegexs,
      BrowserTypes,
      Browsers,
      DeviceRegexs,
      Devices,
      OperatingSystemRegexs,
      OperatingSystems,
      Robots)
    def parse(line: String): Option[KnownSection] = {
      val matches = variants.filter(_.classify(line))
      if (matches.length == 1)
        Some(matches.head)
      else
        None
    }
  }
  case object BrowserOperatingSystems extends KnownSection {
    val id = "browser_os"
  }
  case object BrowserRegexs extends KnownSection {
    val id = "browser_reg"
  }
  case object BrowserTypes extends KnownSection {
    val id = "browser_type"
  }
  case object Browsers extends KnownSection {
    val id = "browser"
  }
  case object DeviceRegexs extends KnownSection {
    val id = "device_reg"
  }
  case object Devices extends KnownSection {
    val id = "device"
  }
  case object OperatingSystemRegexs extends KnownSection {
    val id = "os_reg"
  }
  case object OperatingSystems extends KnownSection {
    val id = "os"
  }
  case object Robots extends KnownSection {
    val id = "robots"
  }

  object Entry {
    private class ParseException(msg: String) extends RuntimeException(msg)

    def apply(section: Section, lines: Seq[Line]): Entry = {
      val entries: Seq[Property] = this.properties(lines)
      val parseFailure: PartialFunction[Throwable, Try[Entry]] = { case _ => Failure(new ParseException(s"Cannot parse entries in section $section: $entries")) }
      val attempt: Try[Entry] = section match {
        case Browsers =>
          mapToBrowser(entries).recoverWith(parseFailure)
        case BrowserOperatingSystems =>
          mapToBrowserToOperatingSystem(entries).recoverWith(parseFailure)
        case BrowserRegexs =>
          mapToBrowserRegex(entries).recoverWith(parseFailure)
        case BrowserTypes =>
          mapToBrowserType(entries).recoverWith(parseFailure)
        case Devices =>
          mapToDevice(entries).recoverWith(parseFailure)
        case DeviceRegexs =>
          mapToDeviceRegex(entries).recoverWith(parseFailure)
        case OperatingSystems =>
          mapToOperatingSystem(entries).recoverWith(parseFailure)
        case OperatingSystemRegexs =>
          mapToOperatingSystemRegex(entries).recoverWith(parseFailure)
        case Robots =>
          mapToRobot(entries).recoverWith(parseFailure)
        case u: UnknownSection => Try {
          FailureType("Entries of unknown sections cannot be mapped")
        }
        case Global => Try {
          FailureType("Entries in global section must be mapped separately")
        }
      }
      attempt match {
        case Success(s) => s
        case Failure(t) => FailureType(t.getLocalizedMessage())
      }
    }

    def extractId(p: Property): Option[Int] = startingDigits(p.name)

    def extractValue(p: Property): String = unquote(p.value)

    def intOption(s: String): Option[Int] =
      if (s.isEmpty) None else Some(s.toInt)

    def enoughEntries(section: Section, entries: Seq[Property]): Boolean =
      entrySize(section) == entries.size

    def entrySize(section: Section): Int = {
      section match {
        case BrowserOperatingSystems => 2
        case BrowserRegexs => 2
        case BrowserTypes => 1
        case Browsers => 7
        case DeviceRegexs => 2
        case Devices => 3
        case Global => 1
        case OperatingSystemRegexs => 2
        case OperatingSystems => 6
        case Robots => 9
        case u: UnknownSection => 1
      }
    }

    def mapToBrowser(entries: Seq[Property]): Try[Browser] = Try {
      Browser(
        BrowserId(extractId(entries(0)).get),
        extractValue(entries(0)).toInt,
        extractValue(entries(1)),
        extractValue(entries(2)),
        extractValue(entries(3)),
        extractValue(entries(4)),
        extractValue(entries(5)),
        extractValue(entries(6)))
    }

    def mapToBrowserRegex(entries: Seq[Property]): Try[BrowserRegex] = Try {
      BrowserRegex(
        extractId(entries(0)).get,
        regexToPattern(extractValue(entries(0))),
        BrowserId(extractValue(entries(1)).toInt))
    }

    def mapToBrowserToOperatingSystem(entries: Seq[Property]): Try[BrowserToOperatingSystem] = Try {
      BrowserToOperatingSystem(
        BrowserId(extractId(entries(0)).get.toInt),
        OperatingSystemId(extractValue(entries(0)).toInt))
    }

    def mapToBrowserType(entries: Seq[Property]): Try[BrowserType] = Try {
      BrowserType(
        extractId(entries(0)).get,
        extractValue(entries(0)))
    }

    def mapToDevice(entries: Seq[Property]): Try[Device] = Try {
      Device(
        DeviceId(extractId(entries(0)).get),
        extractValue(entries(0)),
        extractValue(entries(1)),
        extractValue(entries(2)))
    }

    def mapToDeviceRegex(entries: Seq[Property]): Try[DeviceRegex] = Try {
      DeviceRegex(
        extractId(entries(0)).get,
        regexToPattern(extractValue(entries(0))),
        DeviceId(extractValue(entries(1)).toInt))
    }

    def mapToOperatingSystem(entries: Seq[Property]): Try[OperatingSystem] = Try {
      OperatingSystem(
        OperatingSystemId(extractId(entries(0)).get),
        extractValue(entries(0)),
        extractValue(entries(1)),
        extractValue(entries(2)),
        extractValue(entries(3)),
        extractValue(entries(4)),
        extractValue(entries(5)))
    }

    def mapToOperatingSystemRegex(entries: Seq[Property]): Try[OperatingSystemRegex] = Try {
      OperatingSystemRegex(
        extractId(entries(0)).get,
        regexToPattern(extractValue(entries(0))),
        OperatingSystemId(extractValue(entries(1)).toInt))
    }

    def mapToRobot(entries: Seq[Property]): Try[Robot] = Try {
      Robot(
        RobotId(extractId(entries(0)).get),
        extractValue(entries(0)),
        extractValue(entries(1)),
        extractValue(entries(2)),
        extractValue(entries(3)),
        extractValue(entries(4)),
        extractValue(entries(5)),
        extractValue(entries(6)),
        operatingSystemId(extractValue(entries(7))),
        extractValue(entries(8)))
    }

    def operatingSystemId(s: String): Option[OperatingSystemId] =
      intOption(s).flatMap(o => Some(OperatingSystemId(o)))

    def properties(lines: Seq[Line]): Seq[Property] = {
      val zero = List[Property]()
      lines.foldRight(zero)((ln, acc) => ln match {
        case e: Property => e :: acc
        case _ => acc
      })
    }

    def regexToPattern(regex: String): Pattern = {
      Converter.convertPerlRegexToPattern(regex) // TODO return Option or Try
    }

    /**
     * Filter beginning numbers from input. If the input does not start with
     * digits or the digits could not be parsed into an `Int` the function
     * returns `None`.
     */
    def startingDigits(s: String): Option[Int] = {
      val digits = s.trim.takeWhile(_.isDigit)
      lazy val num = Try(digits.toInt)
      if (!digits.isEmpty && num.isSuccess)
        num.toOption
      else
        None
    }

    /**
     * Trims leading and trailing whitespace characters from input. Additionally
     * surrounding quotes will be removed.
     */
    def unquote(s: String): String = {
      val b = new StringBuilder(s.trim)
      if (b.length > 1 && b.startsWith("\"") && b.endsWith("\""))
        b.deleteCharAt(0).deleteCharAt(b.length - 1)
      b.toString
    }

  }

  private def append(acc: Map[Section, Vector[Line]], ln: Line, section: Section): Map[Section, Vector[Line]] = {
    val types = acc.getOrElse(section, Vector())
    acc + (section -> (types :+ ln))
  }

  private[datasource] def asProperty(ln: Line): Property = ln match {
    case p: Property => p
    case l: Line => throw new Exception(s"$l is not a Property")
  }

  private[datasource] def isPropertyWithName(name: String)(ln: Line): Boolean = ln match {
    case p: Property => p.name == name
    case _ => false
  }

  private[datasource] def firstPropertiesWithName(name: String, lines: Seq[Line]): Seq[Property] = {
    val firstWithName = lines takeWhile isPropertyWithName(name)
    firstWithName map asProperty
  }

  private[datasource] def groupLinesBySection(lines: Seq[Line]): Map[Section, Vector[Line]] = {
    @tailrec
    def go(acc: Map[Section, Vector[Line]], lines: Seq[Line], s: Section): Map[Section, Vector[Line]] = {
      if (lines.isEmpty)
        acc
      else {
        var section = s
        val accumulated = lines.head match {
          case c: Comment =>
            append(acc, c, s)
          case p: Property =>
            append(acc, p, s)
          case s: Section =>
            section = s
            if (acc.get(section).isEmpty)
              acc + (section -> Vector())
            else
              acc
          case u: Unknown =>
            append(acc, u, s)
        }
        go(accumulated, lines.tail, section)
      }
    }
    go(Map(), lines, Global)
  }

  /**
   * Reduces a stream of lines into a data structure for *UAS parser*.
   */
  def linesToData(lines: Seq[String]): Data = {
    val classified = lines map Line.apply
    toData(classified)
  }

  private[datasource] def toData(lines: Seq[Line]): Data = {
    toData {
      groupLinesBySection(lines)
    }
  }

  private[datasource] def toData: Map[Section, Vector[Line]] => Data = _ map {
    case (section: Section, entries: Vector[Line]) =>
      section -> toEntries(section, entries)
  }

  private[datasource] def toEntries(section: Section, lines: Seq[Line]): Vector[Entry] = {
    if (lines.nonEmpty) {
      var entries = lines
      var buf = new ArrayBuffer[Entry]
      while (entries.nonEmpty) {
        entries = entries.head match {
          case c: Comment =>
            buf.append(c)
            entries.tail
          case e: Property => {
            val fields = firstPropertiesWithName(e.name, entries).toVector
            val fieldsLength = fields.length
            if (fieldsLength > 0) {
              buf.append(Entry(section, fields))
              entries.drop(fieldsLength)
            } else {
              buf.append(Unknown(e.name + "=" + e.value))
              entries.tail
            }
          }
          case s: Section =>
            throw new Error("group by section first")
          case u: Unknown =>
            buf.append(u)
            entries.tail
        }
      }
      buf.toVector
    } else Vector()
  }

}