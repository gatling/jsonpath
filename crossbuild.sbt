lazy val scala213 = "2.13.1"
lazy val scala212 = "2.12.10"
lazy val scala211 = "2.11.12"
lazy val supportedScalaVersions = List(scala213, scala212, scala211)

scalaVersion := scala213
crossScalaVersions := supportedScalaVersions

fork in (ThisBuild, Test) := scalaVersion.value.startsWith("2.11.")
