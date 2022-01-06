enablePlugins(ScalaJSPlugin)
name := "picross"

version := "0.1"

scalaVersion := "3.1.0"
scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.10"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % "test"