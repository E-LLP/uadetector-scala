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

import scala.Vector
import scala.util.Success

import com.github.before.uadetector.datasource.IniFormat.Comment
import com.github.before.uadetector.datasource.IniFormat.Global
import com.github.before.uadetector.datasource.IniFormat.Line
import com.github.before.uadetector.datasource.IniFormat.Properties
import com.github.before.uadetector.datasource.IniFormat.Property
import com.github.before.uadetector.datasource.IniFormat.Section
import com.github.before.uadetector.datasource.IniFormat.Unknown
import com.github.before.uadetector.datasource.IniFormat.Version

import scalaz.stream.Process.Process1
import scalaz.stream.Process.await1
import scalaz.stream.Process.emit

object Data {

  private[uadetector] def append(e: Entry, s: Section, acc: Data): Data = {
    val entries = acc.getOrElse(s, Vector())
    acc.updated(s, entries :+ e)
  }

  /**
   * Loads lines of an INI file as data structure for *UAS parser*.
   */
  def load(lines: Seq[String]): Data =
    IniFormat.linesToData(lines)

  private[uadetector] def replaceLast(e: Entry, s: Section, acc: Data): Data = {
    val entries = acc.getOrElse(s, Vector()) match {
      case es :+ last => es :+ e
      case _ => throw new Exception("no last entry available")
    }
    acc.updated(s, entries)
  }

  /**
   * Reduces a stream of lines into a data structure for *UAS parser*. This
   * function is not tail-recursive and could lead to a stack overflow.
   */
  def toData: Process1[String, Data] = {
    def go(acc: Data, s: Section): Process1[String, Data] = {
      await1[String].flatMap { ln =>
        val line = Line(ln)
        line match {
          case c: Comment =>
            go(append(c, s, acc), s)
          case p: Property =>
            val accumulated: Data = acc.getOrElse(s, Vector()) match {
              case es :+ last => last match {
                case Properties(vs) =>
                  val n = vs :+ p
                  val ps = Properties.convert(s, n) match {
                    case Success(v) => v
                    case _ => Properties(n)
                  }
                  replaceLast(ps, s, acc)
                case one: Entry =>
                  val n = Vector(p)
                  val ps = Properties.convert(s, n) match {
                    case Success(v) => v
                    case _ => Properties(n)
                  }
                  append(ps, s, acc)
                case _ => append(Properties(Vector(p)), s, acc)
              }
              case _ => append(Properties(Vector(p)), s, acc)
            }
            go(accumulated, s)
          case section: Section =>
            emit(acc) fby go(acc, section)
          case u: Unknown =>
            go(append(u, s, acc), s)
          case v: Version =>
            go(append(v, s, acc), s)
        }
      } orElse emit(acc)
    }
    go(Map(), Global)
  }

}
