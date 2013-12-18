name := "jsonpath"

organization := "io.gatling"                                        

version := "0.3-SNAPSHOT"                                                       

scalaVersion := "2.10.3"   

scalacOptions ++= Seq("-unchecked", "-deprecation")

/// ScalaTest
libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.3.0" % "test"

/// ScalaMeter
libraryDependencies += "com.github.axel22" %% "scalameter" % "0.4" % "test"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

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
  <url>http://github.com/gatling/jsonpath</url>
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
