ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.7"

lazy val root = (project in file("."))
  .settings(
    name := "TargilZero"
  )

lazy val project1 = (project in file("project-1"))
  .settings(
    name := "project-1",
    scalaVersion := "3.3.7"
  )

lazy val project2 = (project in file("project-2"))
  .dependsOn(project1)
  .settings(
    name := "project-2",
    scalaVersion := "3.3.7"
  )

lazy val project0 = (project in file("project-0"))
  .dependsOn(project2)
  .settings(
    name := "project-0",
    scalaVersion := "3.3.7"
  )