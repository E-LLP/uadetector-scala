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

sealed trait Flag {
  val number: Int
  val character: Char
}
case object CanonicalEquivalence extends Flag {
  val number = java.util.regex.Pattern.CANON_EQ
  val character = 'c'
}
case object CaseInsensitiveMatching extends Flag {
  val number = java.util.regex.Pattern.CASE_INSENSITIVE
  val character = 'i'
}
case object WhitespaceAndComments extends Flag {
  val number = java.util.regex.Pattern.COMMENTS
  val character = 'x'
}
case object DotallMode extends Flag {
  val number = java.util.regex.Pattern.DOTALL
  val character = 's'
}
case object LiteralParsing extends Flag {
  val number = java.util.regex.Pattern.LITERAL
  val character = 'l'
}
case object MultilineMode extends Flag {
  val number = java.util.regex.Pattern.MULTILINE
  val character = 'm'
}
case object UnicodeAwareCaseFolding extends Flag {
  val number = java.util.regex.Pattern.UNICODE_CASE
  val character = 'u'
}
case object UnixLinesMode extends Flag {
  val number = java.util.regex.Pattern.UNIX_LINES
  val character = 'e'
}
case object UnicodeVersionOfCharacterClasses extends Flag {
  val number = java.util.regex.Pattern.UNICODE_CHARACTER_CLASS
  val character = 'U'
}

object FlagOrdering extends Ordering[Flag] {
  def compare(a: Flag, b: Flag) = a.number compare b.number
}

object Flag extends {

  /**
   * Converts a set of flags as to a bitmask (sum of numerical values).
   *
   * @param flags
   *            a set of flags
   * @return sum of numerical values of passed flags or 0
   */
  def convertToBitmask(flags: Set[Flag]): Int =
    flags.foldLeft(0) { (bitmask, flag) =>
      bitmask | flag.number
    }

  /**
   * Converts a set of flags as to a string representation. The flags {@link Flag#CASE_INSENSITIVE},
   * {@link Flag#DOTALL}, {@link Flag#MULTILINE} and {@link Flag#COMMENTS} are identical to the PERL regular
   * expression modifiers.
   *
   * @param flags
   *            a set of flags
   * @return sum of numerical values of passed flags or 0
   */
  def convertToModifiers(flags: Set[Flag]): String = {
    implicit val ordering = FlagOrdering
    flags.toSeq.sorted.reverse.map(flag => flag.character).mkString
  }

  /**
   * This method try to find a matching enum value by the given character representation. The character will be
   * evaluated against the stored character of a flag.
   *
   * @param flag
   *            representation of a flag as a character
   * @return the matching enum value or {@code null}
   * @throws net.sf.qualitycheck.exception.IllegalNegativeArgumentException
   *             if the given number is smaller than zero
   */
  def evaluateByCharacter(flag: Char): Option[Flag] = flag match {
    case CanonicalEquivalence.character => Some(CanonicalEquivalence)
    case CaseInsensitiveMatching.character => Some(CaseInsensitiveMatching)
    case WhitespaceAndComments.character => Some(WhitespaceAndComments)
    case DotallMode.character => Some(DotallMode)
    case LiteralParsing.character => Some(LiteralParsing)
    case MultilineMode.character => Some(MultilineMode)
    case UnicodeAwareCaseFolding.character => Some(UnicodeAwareCaseFolding)
    case UnixLinesMode.character => Some(UnixLinesMode)
    case UnicodeVersionOfCharacterClasses.character => Some(UnicodeVersionOfCharacterClasses)
    case _ => None
  }

  /**
   * This method try to find a matching enum value by the given numerical representation. The number will be
   * evaluated against the stored number of a flag.
   *
   * @param flag
   *            representation of a flag as a character
   * @return the matching enum value or {@code null}
   * @throws net.sf.qualitycheck.exception.IllegalNegativeArgumentException
   *             if the given number is smaller than zero
   */
  def evaluateByNumber(flag: Int): Option[Flag] = flag match {
    case CanonicalEquivalence.number => Some(CanonicalEquivalence)
    case CaseInsensitiveMatching.number => Some(CaseInsensitiveMatching)
    case WhitespaceAndComments.number => Some(WhitespaceAndComments)
    case DotallMode.number => Some(DotallMode)
    case LiteralParsing.number => Some(LiteralParsing)
    case MultilineMode.number => Some(MultilineMode)
    case UnicodeAwareCaseFolding.number => Some(UnicodeAwareCaseFolding)
    case UnixLinesMode.number => Some(UnixLinesMode)
    case UnicodeVersionOfCharacterClasses.number => Some(UnicodeVersionOfCharacterClasses)
    case _ => None
  }

  /**
   * Parses a sum of flags as numerical values (bitmask) and translates it to set of enum values.
   *
   * @param bitmask
   *            Sum of numerical values of flags
   * @return a set of flags
   * @throws net.sf.qualitycheck.exception.IllegalNegativeArgumentException
   *             if the given number is smaller than zero
   */
  def parse(bitmask: Int): Set[Flag] = {
    val flags = new collection.mutable.ArrayBuffer[Flag]()
    if ((bitmask & UnixLinesMode.number) != 0) {
      flags.append(UnixLinesMode)
    }
    if ((bitmask & CaseInsensitiveMatching.number) != 0) {
      flags.append(CaseInsensitiveMatching)
    }
    if ((bitmask & WhitespaceAndComments.number) != 0) {
      flags.append(WhitespaceAndComments)
    }
    if ((bitmask & MultilineMode.number) != 0) {
      flags.append(MultilineMode)
    }
    if ((bitmask & LiteralParsing.number) != 0) {
      flags.append(LiteralParsing)
    }
    if ((bitmask & DotallMode.number) != 0) {
      flags.append(DotallMode)
    }
    if ((bitmask & UnicodeAwareCaseFolding.number) != 0) {
      flags.append(UnicodeAwareCaseFolding)
    }
    if ((bitmask & CanonicalEquivalence.number) != 0) {
      flags.append(CanonicalEquivalence)
    }
    if ((bitmask & UnicodeVersionOfCharacterClasses.number) != 0) {
      flags.append(UnicodeVersionOfCharacterClasses)
    }
    flags.toSet
  }

  /**
   * Translates PERL style modifiers to a set of {@code Pattern} compatible ones.
   *
   * @param modifiers
   *            modifiers as string of a PERL style regular expression
   * @return a set of modifier flags that may include CASE_INSENSITIVE, MULTILINE, DOTALL and COMMENTS
   */
  def parse(modifiers: String): Set[Flag] = {
    val empty = Set[Flag]()
    modifiers.map(flag => evaluateByCharacter(flag)).foldLeft(empty) { (acc, flag) =>
      flag match {
        case Some(f) => acc + f
        case None => acc
      }
    }
  }

}
