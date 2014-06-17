package com.github.before.uadetector

import scala.concurrent.duration.Duration
import scala.io.Codec

import com.github.before.uadetector.Time.measure
import com.github.before.uadetector.datasource.Data
import com.github.before.uadetector.datasource.Entry
import com.github.before.uadetector.datasource.IniFormat.Comment

import scalaz.stream.io

object ShowDataStatistics extends App {

  private def printDuration(d: Duration): Unit = {
    println(s"INI loaded in ${d.toMillis} milliseconds")
  }

  val version = "20140609-03"
  val resource = getClass().getClassLoader().getResource(s"uas_${version}.ini")

  var data: Data = null
  for (i <- 0 to 100) {
    val readLines = io.linesR(resource.getFile)(Codec.UTF8)
    val load = task.loadData(readLines)
    data = measure(load.runLastOr(Map()).run, printDuration)
  }
  //  val data = measure(Data.load(readLines).run, printDuration)

  def print(data: Data) {

    def comments(e: Entry): Boolean = e.isInstanceOf[Comment]

    val filtered = (for {
      entrySet <- data
    } yield (entrySet._1 -> entrySet._2.filterNot(comments(_))))

    val entriesSizeOfGroups = (for {
      entrySet <- filtered
    } yield s"  ${entrySet._1} : ${entrySet._2.size}").toSeq.sorted.mkString("\n")

    println(entriesSizeOfGroups)
  }

  print(data)

}
