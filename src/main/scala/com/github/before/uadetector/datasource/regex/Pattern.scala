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

/**
 * Represents a regular expression that ensures equality by comparing values
 * in contrast to `java.util.regex.Pattern`.
 */
case class Pattern(regex: java.util.regex.Pattern) {

  /**
   * Returns `true` if the specified value is a `Pattern` with the same
   * contents as this one otherwise `false`.
   */
  override def equals(other: Any): Boolean = other match {
    case o: Pattern =>
      o.pattern == pattern &&
        o.flags == flags
    case _ => false
  }

  /** Returns this pattern's match flags. */
  def flags: Int = regex.flags

  override def hashCode: Int =
    util.hashing.MurmurHash3.stringHash(regex.pattern) + regex.flags

  /** Returns the regular expression from which this pattern was compiled. */
  def pattern: String = regex.pattern

}
