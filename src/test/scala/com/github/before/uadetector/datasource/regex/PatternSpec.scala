package com.github.before.uadetector.datasource.regex

import java.util.regex.{Pattern => P}

import org.specs2.mutable.Specification

class PatternSpec extends Specification {

  "equals method" should {
    "return false with different flags" in {
      new Pattern(P.compile("[a-z]+", P.CASE_INSENSITIVE)) must not be equalTo(
        new Pattern(P.compile("[a-z]+", P.CASE_INSENSITIVE + P.DOTALL)))
    }
    "return false with different patterns" in {
      new Pattern(P.compile("[a-z]+", P.CASE_INSENSITIVE)) must not be equalTo(
        new Pattern(P.compile("[0-9]+", P.CASE_INSENSITIVE)))
    }
    "return true with same flags and pattern" in {
      new Pattern(P.compile("[a-z]+", P.CASE_INSENSITIVE)) must be equalTo (
        new Pattern(P.compile("[a-z]+", P.CASE_INSENSITIVE)))
    }
  }

  "hashCode method" should {
    "return different number with different flags" in {
      new Pattern(P.compile("[a-z]+", P.CASE_INSENSITIVE)).hashCode must not be equalTo(
        new Pattern(P.compile("[a-z]+", P.CASE_INSENSITIVE + P.DOTALL)).hashCode)
    }
    "return different number with different patterns" in {
      new Pattern(P.compile("[a-z]+", P.CASE_INSENSITIVE)).hashCode must not be equalTo(
        new Pattern(P.compile("[0-9]+", P.CASE_INSENSITIVE)).hashCode)
    }
    "return same number with same flags and pattern" in {
      new Pattern(P.compile("[a-z]+", P.CASE_INSENSITIVE)).hashCode must be equalTo (
        new Pattern(P.compile("[a-z]+", P.CASE_INSENSITIVE)).hashCode)
    }
  }

}
