name := "dmn-engine-rest"

assemblyJarName in assembly :=  s"${name.value}-${version.value}-full.jar"

enablePlugins(SbtTwirl)
enablePlugins(ScalatraPlugin)
