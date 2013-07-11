name := "jsonpath"

organization := "io.gatling"                                        

version := "0.1"                                                       

scalaVersion := "2.10.2"   

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"

/// JSON-Smart
libraryDependencies += "net.minidev" % "json-smart" % "2.0-RC2"

/// ScalaMeter
resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.github.axel22" %% "scalameter" % "0.4" % "test"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")


/// Jayway JsonPath - for benchmarking purpose
libraryDependencies += "com.jayway.jsonpath" % "json-path" % "0.8.1" % "test"


/// Junit-Benchmark
libraryDependencies += "junit" % "junit" % "4.11" % "test"

libraryDependencies += "com.carrotsearch" % "junit-benchmarks" % "0.6.0" % "test"