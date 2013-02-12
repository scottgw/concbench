import AssemblyKeys._

name := "Condition"

version := "1.0"

scalaVersion := "2.9.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.5"

// Allows the use of JAR assembly
assemblySettings

jarName in assembly := "Main.jar"