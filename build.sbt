autoCompilerPlugins := true
cancelable in Global := true

lazy val commonSettings = Seq(
  organization := "com.github.uw-pharm",
  scalaVersion := "2.12.6"
)

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    version := "0.5",
    name := "bitsad"
  )
  .aggregate(libraries, plugin)

lazy val libraries = (project in file("libraries"))
  .settings(
    commonSettings,
    name := "libraries"
  )
  .dependsOn(macros)

lazy val macros = (project in file("macros"))
  .settings(
    commonSettings,
    name := "macros",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )

lazy val plugin = (project in file("plugin"))
  .settings(
    commonSettings,
    scalacOptions ++= Seq("-J-Xss256m", "-deprecation"),
    name := "bitsad-plugin",
    assemblyJarName in assembly := "bitsad-plugin.jar",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )
  .dependsOn(libraries)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

inThisBuild(List(
    // These are normal sbt settings to configure for release, skip if already defined
    licenses := Seq("GPL-3.0" -> url("http://opensource.org/licenses/GPL-3.0")),
    homepage := Some(url("https://github.com/UW-PHARM/BitSAD")),
    developers := List(Developer("", "$NAME", "$EMAIL", url("$YOUR_WEBSITE"))),
    scmInfo := Some(ScmInfo(url("https://github.com/UW-PHARM/BitSAD.git"), "scm:git:git@github.com:UW-PHARM/BitSAD.git")),

    // These are the sbt-release-early settings to configure
    pgpPublicRing := file("/groups/ece/ececompeng/lipasti/bitsad/pubring.asc"),
    pgpSecretRing := file("/groups/ece/ececompeng/lipasti/bitsad/secring.asc"),
    releaseEarlyWith in Global := SonatypePublisher
))
