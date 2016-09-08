name := "proot-loader"

version := "0.0.2"

scalaVersion := "2.11.8" 
scalacOptions += "-target:jvm-1.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"
libraryDependencies += "net.liftweb" %% "lift-json" % "2.6"

//mainClass in Compile := Some("PRootContainerLoader")

