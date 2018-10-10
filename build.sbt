name := "chess-dojo"

version := "0.1"

scalaVersion := "2.12.7"

unmanagedBase := baseDirectory.value / "ext-lib"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.4.0"

addCompilerPlugin("org.spire-math" %% "kind-projector"     % "0.9.7")