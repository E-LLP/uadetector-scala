package com.github.before.uadetector.datasource

trait DataVersion extends Entry {
  def value: String
}

case object UnknownDataVersion extends DataVersion {
  val value = "(unknown)"
}
