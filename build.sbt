name := "jsonpath"

organization := "io.gatling"                                        

version := "0.1"                                                       

scalaVersion := "2.10.2"   

scalacOptions ++= Seq("-unchecked", "-deprecation")

/// ScalaTest
libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"

/// JSON-Smart
libraryDependencies += "net.minidev" % "json-smart" % "2.0-RC2"

/// ScalaMeter
resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.github.axel22" %% "scalameter" % "0.3" % "test"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")


/// Jayway JsonPath - for benchmarking purpose
libraryDependencies += "com.jayway.jsonpath" % "json-path" % "0.8.1" % "test"


/// Junit-Benchmark
libraryDependencies += "junit" % "junit" % "4.11" % "test"

libraryDependencies += "com.carrotsearch" % "junit-benchmarks" % "0.6.0" % "test"




/// Publishing
publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://github.com/nremond/pbkdf2-scala</url>
  <licenses>
    <license>
      <name>Apache</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:gatling/jsonpath.git</url>
    <connection>scm:git@github.com:gatling/jsonpath.git</connection>
  </scm>
  <developers>
    <developer>
      <id>nremond</id>
      <name>Nicolas Rémond</name>
    </developer>
    <developer>
      <id>slandelle</id>
      <name>Stéphane Landelle</name>
    </developer>
  </developers>
)