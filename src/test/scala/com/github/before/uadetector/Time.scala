package com.github.before.uadetector

import scala.concurrent.duration.Duration

final object Time {

  def measure[A](a: => A): (A, Duration) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    val duration = Duration(micros, "microseconds")
    (result, duration)
  }

  def measure[A](a: => A, c: Duration => Unit): A = {
    val result = measure(a)
    c(result._2)
    result._1
  }

}
