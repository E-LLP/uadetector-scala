package com.github.before.uadetector.datasource

import scala.Vector

import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import com.github.before.uadetector.datasource.IniFormat._

object EntrySpec extends Specification with ScalaCheck {

  "linesToData function" should {
    "maps an entry set of a single robot in a robots section" in {
      val lines = List(
        Robots,
        Property("1=uas"),
        Property("1=family"),
        Property("1=name"),
        Property("1=url"),
        Property("1=company"),
        Property("1=company url"),
        Property("1=icon name"),
        Property("1="),
        Property("1=info url"))
      IniFormat.toData(lines) must equalTo(
        Map(
          Robots -> Vector(
            Robot(
              RobotId(1),
              "uas",
              "family",
              "name",
              "url",
              "company",
              "company url",
              "icon name",
              None,
              "info url"))))
    }
    "maps two entry sets of a robots in a robots section" in {
      val lines = List(
        Robots,
        Property("1=uas"),
        Property("1=family"),
        Property("1=name"),
        Property("1=url"),
        Property("1=company"),
        Property("1=company url"),
        Property("1=icon name"),
        Property("1="),
        Property("1=info url"),
        Property("2=uas"),
        Property("2=family"),
        Property("2=name"),
        Property("2=url"),
        Property("2=company"),
        Property("2=company url"),
        Property("2=icon name"),
        Property("2="),
        Property("2=info url"))
      IniFormat.toData(lines) must equalTo(
        Map(
          Robots -> Vector(
            Robot(RobotId(1), "uas", "family", "name", "url", "company", "company url", "icon name", None, "info url"),
            Robot(RobotId(2), "uas", "family", "name", "url", "company", "company url", "icon name", None, "info url"))))
    }
    "maps some global comments before a section starts" in {
      val lines = List(
        Comment("a comment"),
        Comment("another comment"),
        Robots)
      IniFormat.toData(lines) must equalTo(
        Map(
          Global -> Vector(
            Comment("a comment"),
            Comment("another comment")),
          Robots -> Vector()))
    }
  }

  "A list of entries for a robot" should {
    "map with a complete list but without an operating system ID to a robot" in {
      val entries = List(
        Property("1=uas"),
        Property("1=family"),
        Property("1=name"),
        Property("1=url"),
        Property("1=company"),
        Property("1=company url"),
        Property("1=icon name"),
        Property("1="),
        Property("1=info url"))
      Entry(Robots, entries) must equalTo(
        Robot(
          RobotId(1),
          "uas",
          "family",
          "name",
          "url",
          "company",
          "company url",
          "icon name",
          None,
          "info url"))
    }
    "map with a complete list with non-empty values to a robot" in {
      val entries = List(
        Property("1=uas"),
        Property("1=family"),
        Property("1=name"),
        Property("1=url"),
        Property("1=company"),
        Property("1=company url"),
        Property("1=icon name"),
        Property("1=82"),
        Property("1=info url"))
      Entry(Robots, entries) must equalTo(
        Robot(
          RobotId(1),
          "uas",
          "family",
          "name",
          "url",
          "company",
          "company url",
          "icon name",
          Some(OperatingSystemId(82)),
          "info url"))
    }
    "not map with an incomplete list" in {
      val segment = Robots
      val entries = List(
        Property("1=uas"),
        Property("1=family"),
        Property("1=name"),
        Property("1=url"),
        Property("1=company"),
        Property("1=company url"),
        Property("1=icon name"),
        Property("1=82"))
      Entry(segment, entries) must haveClass[FailureType]
    }
  }

  "startingDigits function" should {
    "evaluate an blank input" in {
      Entry.startingDigits(" ") must equalTo(None)
    }
    "evaluate an empty input" in {
      Entry.startingDigits("") must equalTo(None)
    }
    "filter nothing if a string does not start with digits" in {
      Entry.startingDigits("some1234") must equalTo(None)
    }
    "filter digits if a string starts with them" in {
      Entry.startingDigits("1234some") must equalTo(Option(1234))
    }
    "filter digits only at the beginning" in {
      Entry.startingDigits("1234 5678") must equalTo(Option(1234))
    }
    "ignore whitespace characters at the beginning when filtering numbers" in {
      Entry.startingDigits("   1234some") must equalTo(Option(1234))
    }
    "parse digits into Int only if not bigger than Int.MaxValue" in {
      Entry.startingDigits("9876543210") must equalTo(None)
    }
  }

  "startingDigits function with arbitrary data" should {
    {
      implicit val arbNumStr: Arbitrary[String] = Arbitrary(Generator.intStartingStr)
      "parse digits always if not bigger than Int.MaxValue" in prop { (s: String) =>
        Entry.startingDigits(s) != None
      }
    }
    {
      implicit val arbNumStr: Arbitrary[String] = Arbitrary(Generator.numStartingStrWithWs)
      "never parse if number is bigger than Int.MaxValue" in prop { (s: String) =>
        Entry.startingDigits(s) == None
      }
    }
  }

  "unquote function" should {
    "ignore surrounding quotes if trailing quote not exists" in {
      Entry.unquote("\"some value") must equalTo("\"some value")
    }
    "ignore surrounding quotes if leading quote not exists" in {
      Entry.unquote("some value\"") must equalTo("some value\"")
    }
    "remove leading and trailing whitespace" in {
      Entry.unquote(" some value ") must equalTo("some value")
    }
    "remove surrounding quotes" in prop { (s: String) =>
      Entry.unquote("\"some value\"") must equalTo("some value")
    }
    "remove surrounding quotes only once" in prop { (s: String) =>
      Entry.unquote("\"\"some value\"\"") must equalTo("\"some value\"")
    }
    "remove all leading and trailing whitespace and surrounding quotes one time" in {
      Entry.unquote("   \"\"some value \"      ") must equalTo("\"some value ")
    }
    "remove all leading and trailing whitespace and surrounding quotes one time without an inner value" in {
      Entry.unquote("   \"\"      ") must equalTo("")
    }
    "remove all leading and trailing whitespace without quotes and inner value" in {
      Entry.unquote("         ") must equalTo("")
    }
    "work with empty value" in {
      Entry.unquote("") must equalTo("")
    }
    "work with one quote only" in {
      Entry.unquote("\"") must equalTo("\"")
    }
  }

  "unquote function with arbitrary data" should {
    {
      implicit val arbEntryValue: Arbitrary[String] = Arbitrary(Generator.entryValue)
      "never throws an exception" in prop { (s: String) =>
        Entry.unquote(s) == Entry.unquote(s)
      }
    }
  }

}
