val shared = Seq(
  organization := "org.camunda.bpm.extension.dmn.scala",
  version := "1.1.0-SNAPSHOT",
  scalaVersion := "2.12.4",
  resolvers += Resolver.mavenLocal,
  resolvers += Classpaths.typesafeReleases,
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

val feelVersion = "1.6.0"
val camundaVersion = "7.9.0"
val zeebeVersion = "0.13.0"
val scalatraVersion = "2.6.2"

lazy val root = (project in file("."))
  .settings(shared)
  .aggregate(engine,
             camundaPlugin,
             standaloneEngine,
             engineRest,
             zeebeWorker,
             benchmark)

lazy val engine = (project in file("dmn-engine"))
  .settings(
    shared,
    name := "dmn-engine",
    description := "DMN engine",
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "org.camunda.bpm.extension.feel.scala" % "feel-engine" % feelVersion,
      "org.camunda.bpm.model" % "camunda-dmn-model" % camundaVersion
    )
  )

lazy val camundaPlugin = (project in file("camunda-plugin"))
  .enablePlugins(AssemblyPlugin)
  .settings(
    shared,
    name := "dmn-engine-camunda-plugin",
    description := "Camunda plugin for the DMN engine",
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "org.camunda.bpm.extension.feel.scala" % "feel-engine-factory" % feelVersion,
      "org.camunda.bpm" % "camunda-engine" % camundaVersion % "provided",
      "com.h2database" % "h2" % "1.4.193" % "test",
      "org.camunda.spin" % "camunda-spin-dataformat-all" % "1.5.0" % "test"
    ),
    assemblyJarName in assembly := s"${name.value}-${version.value}-full.jar"
  )
  .dependsOn(
    engine % "test->test;compile->compile"
  )

lazy val standaloneEngine = (project in file("standalone-engine"))
  .settings(
    shared,
    name := "dmn-engine-standalone",
    description := "Base for standalone DMN engine",
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(
    engine % "test->test;compile->compile"
  )

lazy val engineRest = (project in file("engine-rest"))
  .enablePlugins(AssemblyPlugin, SbtTwirl, ScalatraPlugin)
  .settings(
    shared,
    name := "dmn-engine-rest",
    description := "REST API for the DMN engine",
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "org.scalatra" %% "scalatra" % scalatraVersion,
      "org.scalatra" %% "scalatra-json" % scalatraVersion,
      "org.json4s" %% "json4s-jackson" % "3.5.2",
      "org.eclipse.jetty" % "jetty-webapp" % "9.4.8.v20171121" % "container;compile",
      "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided",
      "org.scalatra" %% "scalatra-scalatest" % scalatraVersion % "test"
    ),
    assemblyJarName in assembly := s"${name.value}-${version.value}-full.jar"
  )
  .dependsOn(
    standaloneEngine % "test->test;compile->compile"
  )

lazy val zeebeWorker = (project in file("zeebe-worker"))
  .enablePlugins(AssemblyPlugin)
  .settings(
    shared,
    name := "dmn-engine-zeebe-worker",
    description := "Zeebe worker for the DMN engine",
    libraryDependencies ++= commonDependencies,
    libraryDependencies ++= Seq(
      "io.zeebe" % "zeebe-client-java" % zeebeVersion,
      "org.apache.logging.log4j" % "log4j-api" % "2.9.0",
      "org.apache.logging.log4j" % "log4j-core" % "2.9.0",
      "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.9.0",
      "io.zeebe" % "zeebe-test" % zeebeVersion % "test",
      "io.zeebe" % "zeebe-broker-core" % zeebeVersion % "test"
    )
  )
  .dependsOn(
    standaloneEngine % "test->test;compile->compile"
  )

lazy val benchmark = (project in file("engine-benchmark"))
  .enablePlugins(JmhPlugin)
  .settings(
    shared,
    name := "dmn-engine-benchmark",
    description := "Benchmark for the DMN engine",
    libraryDependencies ++= Seq(
      "org.apache.logging.log4j" % "log4j-api" % "2.9.0",
      "org.apache.logging.log4j" % "log4j-core" % "2.9.0",
      "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.9.0"
    )
  )
  .dependsOn(
    engine % "test->test;compile->compile"
  )
