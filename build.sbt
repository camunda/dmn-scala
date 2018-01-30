
lazy val commonSettings = Seq(
  organization := "org.camunda.bpm.extension.dmn.scala",
  version := "1.0.0-SNAPSHOT",
  scalaVersion := "2.12.4",

  resolvers += Resolver.mavenLocal,
  resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  resolvers += "camunda-bpm-nexus" at "https://app.camunda.com/nexus/content/groups/public",

  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
)

val commonDependencies = Seq(
  "org.slf4j" % "slf4j-api" % "1.7.25",

  "junit" % "junit" % "4.11" % "test",
  "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test",
  "org.apache.logging.log4j" % "log4j-api" % "2.9.0" % "test",
  "org.apache.logging.log4j" % "log4j-core" % "2.9.0" % "test",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.9.0" % "test"
)

val feelVersion = "1.4.0-SNAPSHOT"
val camundaVersion = "7.9.0-SNAPSHOT"

lazy val root = (project in file(".")).
  settings(commonSettings).
  aggregate(engine, camundaPlugin)

lazy val engine = (project in file("dmn-engine")).
  settings(commonSettings).
  settings(
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "org.camunda.bpm.extension.feel.scala" % "feel-engine" % feelVersion,
      "org.camunda.bpm.model" % "camunda-dmn-model" % camundaVersion
    )
  )
  
lazy val camundaPlugin = (project in file("camunda-plugin")).
  settings(commonSettings).
  settings(
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "org.camunda.bpm" % "camunda-engine" % camundaVersion % "provided",

      "com.h2database" % "h2" % "1.4.193" % "test"
    )
  ).
  dependsOn(
    engine % "test->test;compile->compile"
  )
