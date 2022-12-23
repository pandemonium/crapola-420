name         := "CP 86"
version      := "0.1"
scalaVersion := "3.2.1"
javaOptions in run ++= Seq("-XstartOnFirstThread")
fork in run := true

libraryDependencies ++= {
  val version = "3.3.1"
  val os = "macos-arm64"

  Seq(
    "org.scalactic"     %% "scalactic"       % "3.2.14",
    "org.scalatest"     %% "scalatest"       % "3.2.14"   % "test",
    "org.scalatestplus" %% "scalacheck-1-17" % "3.2.14.0" % "test",
  ) ++ Seq(
    "lwjgl",
    "lwjgl-glfw",
    "lwjgl-opengl"
  ).flatMap { module =>
      Seq(
        "org.lwjgl" % module % version,
        "org.lwjgl" % module % version classifier s"natives-$os"
      )
  }
}