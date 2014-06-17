package com.github.before.uadetector.datasource

import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import com.github.before.uadetector.datasource.IniFormat._

object LineSpec extends Specification with ScalaCheck {

  "apply function" should {
    "determine an entry if a string starts with a number followed by a equals sign" in {
      Line("1234=") must equalTo(Property("1234="))
    }
    "determine an entry if a string starts with a number followed by a equals sign and a some characters" in {
      Line("1234=some value") must equalTo(Property("1234=some value"))
    }
    "determine an entry if the equals sign is surrounded by some whitespace" in {
      Line("1234    =     some value") must equalTo(Property("1234    =     some value"))
      Line("1234 = some value") must equalTo(Property("1234 = some value"))
      Line("1234 =some value") must equalTo(Property("1234 =some value"))
      Line("1234= some value") must equalTo(Property("1234= some value"))
    }
    "determine an entry if the key starts with a number followed by some else before the equals sign, ignore these characters" in {
      Line("1234[]=some value") must be equalTo Property("1234[]=some value")
      Line("1234[] = some value") must be equalTo Property("1234[] = some value")
      Line("1234some=some value") must be equalTo Property("1234some=some value")
      Line("1234some = some value") must be equalTo Property("1234some = some value")
      Line("1234 some=some value ") must be equalTo Property("1234 some=some value ")
      Line("1234 some = some value ") must be equalTo Property("1234 some = some value ")
    }
    "determine a string starting with a semicolon as comment" in {
      Line("; a comment") must be equalTo Comment(" a comment")
    }
    "determine a string starting with a semicolon only as comment" in {
      Line(";") must be equalTo Comment("")
    }
    "determine a string surrounded by brackets as section" in {
      Line("[some section]") must equalTo(UnknownSection("some section"))
    }
    "determine a string surrounded by brackets with a known name as section" in {
      Line("[robots]") must be equalTo Robots
    }
    "not determine a number followed by some characters as an entry" in {
      Line("1234some") must equalTo(Unknown("1234some"))
    }
    "not determine a number only as an entry" in {
      Line("1234") must equalTo(Unknown("1234"))
    }
    "not determine a string starting with a bracket only as section" in {
      Line("[no section") must equalTo(Unknown("[no section"))
    }
    "not determine a string starting with whitespace first followed by a semicolon as comment" in {
      Line(" ;") must equalTo(Unknown(" ;"))
    }
    "not determine a string starting with whitespace first followed by some characters surrounded by brackets as section" in {
      Line(" [some section]") must equalTo(Unknown(" [some section]"))
    }
  }

}
