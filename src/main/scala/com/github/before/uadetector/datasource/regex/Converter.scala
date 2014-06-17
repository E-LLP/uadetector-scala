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
package com.github.before.uadetector.datasource.regex

import java.util.regex.{Pattern => P}

object Converter {

  /**
   * Template to support the conversion into a PERL style regular expression
   */
  private val patternToRegexTemplate = "/%s/%s"

  /**
   * Pattern for PERL style regular expression strings
   */
  private val perlStyle = P.compile("^/.*/((i|m|s|x)*)?$")

  /**
   * Pattern for PERL style regular expression strings with more fault-tolerance to the modifiers
   */
  private val perlStyleTolerant = P.compile("^/.*/(([A-z])*)?$")

  /**
   * Converts a given {@code Pattern} into a PERL style regular expression.
   *
   * @param pattern
   *            regular expression pattern
   * @return PERL style regular expression as string
   */
  def convertPatternToPerlRegex(pattern: P): String = {
    val modifiers = Flag.convertToModifiers(Flag.parse(pattern.flags))
    String.format(patternToRegexTemplate, pattern.pattern(), modifiers)
  }

  /**
   * Converts a PERL style regular expression into Java style.<br>
   * <br>
   * The leading and ending slash and the modifiers will be removed.
   *
   * @param regex
   *            A PERL style regular expression
   * @param faultTolerant
   *            Fault-tolerant translating the flags
   * @return Pattern
   */
  def convertPerlRegexToPattern(regex: String, faultTolerant: Boolean = false): Pattern = {
    var pattern = regex.trim
    val matcher =
      if (faultTolerant)
        perlStyleTolerant.matcher(pattern)
      else
        perlStyle.matcher(pattern)
    if (!matcher.matches) {
      throw new IllegalArgumentException(s"The given regular expression '$pattern' seems to be not in PERL style or has unsupported modifiers.")
    }
    pattern = pattern.substring(1)
    val lastIndex = pattern.lastIndexOf('/')
    pattern = pattern.substring(0, lastIndex)
    val flags = Flag.convertToBitmask(Flag.parse(matcher.group(1)))
    Pattern(P.compile(pattern, flags))
  }

}