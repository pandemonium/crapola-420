name         := "CP 86"
version      := "0.1"
scalaVersion := "3.2.1"
javaOptions in run ++= Seq("-XstartOnFirstThread")
fork in run := true

libraryDependencies ++= {
  val version = "3.3.1"
  val os = "macos-arm64"

  Seq(
    "lwjgl",
    "lwjgl-glfw",
    "lwjgl-opengl"
    // TODO: Add more modules here
  ).flatMap {
    module => {
      Seq(
        "org.lwjgl" % module % version,
        "org.lwjgl" % module % version classifier s"natives-$os"
      )
    }
  }
}