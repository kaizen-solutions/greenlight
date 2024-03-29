name := "green-light"

version := "0.1"

scalaVersion := "2.13.3"
crossScalaVersions := List("2.12.12")

scalacOptions ++= Seq(
  "-deprecation"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.1.1",
  "com.chuusai"   %% "shapeless" % "2.3.3"
)
