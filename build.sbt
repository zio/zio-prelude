import BuildHelper._

inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://zio.github.io/zio-prelude/")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      )
    ),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    scmInfo := Some(
      ScmInfo(url("https://github.com/zio/zio-prelude/"), "scm:git:git@github.com:zio/zio-prelude.git")
    )
  )
)

addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll; compile:scalafix --check; test:scalafix --check")

addCommandAlias(
  "testJVM",
  ";coreJVM/test;experimentalJVM/test"
)
addCommandAlias(
  "testJS",
  ";coreJS/test;experimentalJVM/test"
)
addCommandAlias(
  "testNative",
  ";coreNative/test:compile;experimentalJVM/test:compile"
)

val zioVersion = "1.0.4"

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    benchmarks,
    coreJS,
    coreJVM,
    coreNative,
    docs,
    experimentalJS,
    experimentalJVM,
    experimentalNative
  )

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("core"))
  .settings(stdSettings("zio-prelude"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.prelude"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings( // 2.13 and Dotty standard library doesn't contain Parallel Scala collections
    libraryDependencies ++= {
      val spc = List("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0" % Optional)
      Seq(
        "dev.zio" %%% "zio"      % zioVersion,
        "dev.zio" %%% "zio-test" % zioVersion
      ) ++
        (scalaVersion.value match {
          case BuildHelper.Scala213   => spc
          case BuildHelper.ScalaDotty => spc.map(_.withDottyCompat(scalaVersion.value))
          case _                      => List()
        })
    }
  )
//  .settings(
//    libraryDependencies ++= {
//      val spc = scalaVersion.value match {
//        case BuildHelper.Scala213 | BuildHelper.ScalaDotty =>
//          // 2.13 and Dotty standard library doesn't contain Parallel Scala collections
//          List("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0" % Optional)
//        case _                                             =>
//          List()
//      }
//      Seq(
//        "dev.zio" %%% "zio"      % zioVersion,
//        "dev.zio" %%% "zio-test" % zioVersion
//      ) ++ spc
//    }
//  )
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .enablePlugins(BuildInfoPlugin)

lazy val coreJS     = core.js
  .settings(jsSettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val coreJVM    = core.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val coreNative = core.native
  .settings(nativeSettings)
  .disablePlugins(
    ScalafixPlugin // for some reason `ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value)` isn't enough
  )

lazy val experimental = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("experimental"))
  .dependsOn(core)
  .settings(stdSettings("zio-prelude-experimental"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.prelude.experimental"))
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .enablePlugins(BuildInfoPlugin)

lazy val experimentalJS     = experimental.js
  .settings(jsSettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val experimentalJVM    = experimental.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val experimentalNative = experimental.native
  .settings(nativeSettings)
  .disablePlugins(
    ScalafixPlugin // for some reason `ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value)` isn't enough
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(stdSettings("zio-prelude-benchmarks"))
  .settings(
    publish / skip := true,
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.3.1"
    )
  )
  .dependsOn(coreJVM)
  .enablePlugins(JmhPlugin)

lazy val docs = project
  .in(file("zio-prelude-docs"))
  .settings(stdSettings("zio-prelude-docs"))
  .settings(
    publish / skip := true,
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(coreJS, coreJVM /*, coreNative */ ),
    ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .dependsOn(coreJS, coreJVM /*, coreNative */ )
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)
