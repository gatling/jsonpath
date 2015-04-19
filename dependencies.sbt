libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    // if scala 2.11+ is used, add dependency on scala-parser-combinators module
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3")
    case _ => Seq.empty
  }
}

libraryDependencies += "org.scalatest"              %% "scalatest"        % "2.2.0"  % "test"
libraryDependencies += "com.fasterxml.jackson.core"  % "jackson-databind" % "2.5.2"  % "test"
libraryDependencies += "com.github.axel22"          %% "scalameter"       % "0.5-M2" % "test"