package com.github.before.uadetector.datasource

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import com.github.before.uadetector.datasource.IniFormat._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary

object IniFormatSpec extends Specification with ScalaCheck {

  "A classification of a comment" should {
    "be true if a string starts with a semicolon" in {
      Comment.classify("; comment text") must beTrue
    }
    "be false if a string starts with a whitespace before a semicolon" in {
      Comment.classify(" ; comment text") must beFalse
    }
    "be false if the input is empty" in {
      Comment.classify("") must beFalse
    }
  }

  "A classification of a property" should {
    "be true if a string contains at least one char before an equal sign" in {
      Property.classify("0=") must beTrue
      Property.classify("a=") must beTrue
      Property.classify("some=") must beTrue
      Property.classify(" =") must beTrue
      Property.classify("a=b") must beTrue
    }
    "be false if a string contains an equal sign only" in {
      Comment.classify("=") must beFalse
    }
    "be false if the input is empty" in {
      Property.classify("") must beFalse
    }
  }

  "A classification of a section" should {
    "be true if a string starts and ends with square brackets" in {
      Section.classify("[section]") must beTrue
    }
    "be false if a string starts with a square bracket but does not ends with a closing one" in {
      Section.classify("[section") must beFalse
      Section.classify("[section]more") must beFalse
    }
    "be false if a string starts with some text before a section definition" in {
      Section.classify("text[section]") must beFalse
      Section.classify("some text[section]") must beFalse
    }
    "be false if a string starts with a whitespace before a square bracket" in {
      Section.classify(" [section]") must beFalse
      Section.classify("   [section]") must beFalse
      Section.classify("\t[section]") must beFalse
    }
    "be false if the input is empty" in {
      Section.classify("") must beFalse
    }
  }

  "When parsing a property it" should {
    "result in some property if a string contains at least one char before an equal sign" in {
      Property.parse("0=") must be equalTo Some(Property("0="))
      Property.parse("a=") must be equalTo Some(Property("a="))
      Property.parse("key=") must be equalTo Some(Property("key="))
      Property.parse(" =") must be equalTo Some(Property(" ="))
      Property.parse("a=b") must be equalTo Some(Property("a=b"))
      Property.parse("key=value") must be equalTo Some(Property("key=value"))
      Property.parse("key = value") must be equalTo Some(Property("key = value"))
      Property.parse(" key = value ") must be equalTo Some(Property(" key = value "))
    }
    "result in no value if a string contains an equal sign only" in {
      Property.parse("=") must be equalTo None
    }
    "result in no value if a string contains no equal sign" in {
      Property.parse("0") must be equalTo None
      Property.parse("123") must be equalTo None
      Property.parse("a") must be equalTo None
      Property.parse("key") must be equalTo None
      Property.parse("key value") must be equalTo None
      Property.parse("key:value") must be equalTo None
      Property.parse("key,value") must be equalTo None
    }
  }

}