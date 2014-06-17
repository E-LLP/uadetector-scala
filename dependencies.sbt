resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "org.scalaz.stream" %% "scalaz-stream" % "0.4.1",
  "ch.qos.logback" % "logback-classic" % "1.1.2" % "test",
  "net.sf.uadetector" % "uadetector-resources" % "2014.05" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
  "org.specs2" %% "specs2" % "2.3.12" % "test"
)
