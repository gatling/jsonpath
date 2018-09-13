import _root_.io.gatling.build.MavenPublishKeys._
import _root_.io.gatling.build.license._

import de.heikoseeberger.sbtheader.HeaderPlugin.autoImport._

enablePlugins(AutomateHeaderPlugin, SonatypeReleasePlugin)

val devs = Seq(
  GatlingDeveloper("slandelle@gatling.io", "Stéphane Landelle", isGatlingCorp = true),
  GatlingDeveloper("nremond@gmail.com", "Nicolas Rémond", isGatlingCorp = false)
)

projectDevelopers := devs

headerLicense := ApacheV2License

useSonatypeRepositories := true

githubPath := "gatling/jsonpath"

scalaVersion := "2.12.6"

crossPaths := true
