name := "engine-rest"

assemblyJarName in assembly :=  s"${name.value}-${version.value}.jar"

enablePlugins(SbtTwirl)
enablePlugins(ScalatraPlugin)
