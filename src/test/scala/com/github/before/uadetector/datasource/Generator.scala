package com.github.before.uadetector.datasource

import org.scalacheck.Gen
import org.scalacheck.Gen._

object Generator {

  /** Generates an alpha, numeric, whitespace or quote character */
  def alphaQuWsEqChar = frequency(
    (10, alphaChar),
    (3, numChar),
    (1, quoteChar),
    (1, equalsSign),
    (2, whitespaceChar))

  /** Generates a equal character */
  def equalsSign: Gen[Char] = Gen.const(unicode.equalsSign)

  object unicode {

    /** Generates a Basic Latin character but without digits */
    def basicLatinWithoutDigits: Gen[Char] =
      Gen.oneOf(Gen.choose('\u0020', '\u002F'), Gen.choose('\u003A', '\u007E')).suchThat(!_.isDigit)

    /** Generates a Basic Latin character */
    def basicLatin: Gen[Char] = Gen.choose('\u0020', '\u007E')

    /** Equals sign character */
    def equalsSign: Char = '\u003D'

    /**
     * Generates a Latin-1 Supplement character without No-break space and
     * Soft hyphen character
     */
    def latin1SupplementPrintable: Gen[Char] =
      Gen.oneOf(Gen.choose('\u00A1', '\u00AC'), Gen.choose('\u00AE', '\u00FF'))
        .suchThat(c => c != noBreakSpace && c != softHyphenSpace)

    /** Generates a Latin-1 Supplement character */
    def latin1Supplement: Gen[Char] = Gen.choose('\u00A0', '\u00FF')

    /** Generates a Latin extended A character */
    def latinExtendedA: Gen[Char] = Gen.choose('\u0100', '\u017F')

    /** Generates a Latin extended B character */
    def latinExtendedB: Gen[Char] = Gen.choose('\u0180', '\u024F')

    /** No-break space character is not printable */
    val noBreakSpace: Char = '\u00A0'

    /** Soft hyphen character is not printable */
    val softHyphenSpace: Char = '\u00AD'

  }

  /** Generates a equal character */
  def nonDigitChar: Gen[Char] =
    frequency(
      (50, unicode.basicLatinWithoutDigits),
      (2, unicode.latin1SupplementPrintable),
      (1, unicode.latinExtendedA),
      (1, unicode.latinExtendedB))
      .suchThat(!_.isDigit)

  /** Generates a quote character */
  def quoteChar: Gen[Char] = Gen.const('\"')

  /** Generates a whitespace character */
  def whitespaceChar: Gen[Char] = Gen.const(' ')

  /** Generates alphanumeric characters */
  def alphaNumStr: Gen[String] =
    listOf(alphaNumChar).map(_.mkString).suchThat(_.forall(c => c.isDigit || c.isLetter))

  /** Generates an alpha, numeric, whitespace or quote character */
  def alphaQuWsEqStr = Gen.listOf(alphaQuWsEqChar).map(_.mkString)
    .suchThat(_.forall(c => c.isLetter || c.isDigit || c.isWhitespace || c == '\"' || c == '='))

  /** Generates characters but never with digits */
  def withoutDigitsStr: Gen[String] = listOf(nonDigitChar).map(_.mkString)

  /** Generates at least one */
  def numStr(min: Int): Gen[String] =
    listOfN(min, numChar).map(_.mkString).suchThat(_.forall(_.isDigit))

  /** Generates a quote */
  def quoteStr: Gen[String] =
    listOf(quoteChar).map(_.mkString).suchThat(_.forall(_ == '\"'))

  /** Generates a whitespace */
  def whitespaceStr: Gen[String] =
    listOf(whitespaceChar).map(_.mkString).suchThat(_.forall(_.isWhitespace))

  /**
   * Generates a string that maybe starts with some whitespace and/or a quote
   * followed by some alphanumerical characters, whitespace or quote and maybe
   * ends with a quote and maybe some whitespace.
   */
  def entryValue: Gen[String] = for {
    leadingWs <- whitespaceStr
    quote <- quoteStr
    inner <- alphaQuWsEqStr
    trailingWs <- whitespaceStr
  } yield (leadingWs + quote + inner + quote + trailingWs)

  /**
   * Generates a string that maybe starts with some whitespace characters
   * followed by a random `Int` and ends maybe with some random characters.
   */
  def intStartingStr: Gen[String] = for {
    leadingWs <- whitespaceStr
    number <- Gen.choose(1, Int.MaxValue)
    separator <- alphaChar
    end <- alphaNumStr
  } yield (leadingWs + number.toString + separator + end)

  /**
   * Generates a string that maybe starts with some whitespace characters
   * followed by some numerical characters bigger than `Int.MaxValue` and
   * ends maybe with some random characters.
   */
  def keyValue: Gen[String] = for {
    key <- intStartingStr
    keySuffix <- identifier
    ws <- whitespaceStr
    separator <- Gen.const('=')
    value <- entryValue
  } yield (key + keySuffix + ws + separator + value)

  /**
   * Generates a string that starts with a number bigger than `Int.MaxValue`
   * and ends maybe with some random characters.
   */
  def numStartingStr: Gen[String] = for {
    number <- Gen.choose(0L, Long.MaxValue)
    end <- withoutDigitsStr
  } yield (number + end)

  /**
   * Generates a string that maybe starts with some whitespace characters
   * followed by a number bigger than `Int.MaxValue` and ends maybe with some
   * random characters.
   */
  def numStartingStrWithWs: Gen[String] = for {
    leadingWs <- whitespaceStr
    number <- Gen.choose(Int.MaxValue.toLong + 1, Long.MaxValue)
    end <- withoutDigitsStr
  } yield (leadingWs + number + end)

}
